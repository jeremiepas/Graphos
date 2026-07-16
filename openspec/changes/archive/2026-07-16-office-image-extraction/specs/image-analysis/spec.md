## ADDED Requirements

### Requirement: Image analysis via multimodal LLM
The system SHALL analyze image files (.png, .jpg, .jpeg, .webp, .gif) by sending them as base64-encoded data URLs to a configurable multimodal LLM (default: qwen3.6-moe). The LLM response SHALL be parsed to extract a free-text description, a list of structured entities (label + type), and an image kind classification.

#### Scenario: Standalone image produces description and entity nodes
- **WHEN** a .png resume image is processed with vision enabled
- **THEN** the system produces an ImageFile node with the filename, a description node linked via Contains, and typed entity nodes (e.g., Person "John Doe", Skill "Python") linked via Contains edges with confidence scores

#### Scenario: Vision disabled produces stub node
- **WHEN** vision is disabled in config (`vision.enabled: false`)
- **THEN** the system produces a single stub ImageFile node with nodeKind "Image" and no LLM call is made

#### Scenario: LLM unavailable produces stub with warning
- **WHEN** the vision LLM API call fails (connection refused, timeout, invalid response)
- **THEN** the system produces a stub ImageFile node and logs a warning, without crashing the pipeline

### Requirement: Image kind classification
The system SHALL classify each analyzed image into one of: Photo, Screenshot, Diagram, Resume, Chart, Other. The classification SHALL be stored in `nodeExtra` as the `kind` field.

#### Scenario: Resume image classified correctly
- **WHEN** a resume image is analyzed
- **THEN** the image node's nodeExtra contains `"kind": "resume"`

#### Scenario: Architecture diagram classified correctly
- **WHEN** an architecture diagram image is analyzed
- **THEN** the image node's nodeExtra contains `"kind": "diagram"`

### Requirement: Embedded image extraction from office documents
The system SHALL extract embedded images from PPTX and DOCX files, analyze them using the same vision pipeline, and create ImageFile nodes linked to their parent document via Contains edges.

#### Scenario: PPTX with embedded image
- **WHEN** a .pptx file contains an embedded image on slide 2
- **THEN** the system produces an ImageFile node for the image, a Contains edge from the slide-2 header node to the image node, and entity nodes extracted from the image

#### Scenario: DOCX with embedded image
- **WHEN** a .docx file contains an embedded image
- **THEN** the system produces an ImageFile node for the image, a Contains edge from the file node to the image node, and entity nodes extracted from the image

### Requirement: Batched image processing with GC
The system SHALL process images in batches (configurable, default 5) with `evaluate` + `performGC` between batches to release base64-encoded data and LLM response buffers. This matches the existing chunked extraction pattern.

#### Scenario: 20 images processed in 4 batches
- **WHEN** 20 images are detected and batchSize is 5
- **THEN** the system processes 4 batches of 5 images each, calling performGC between batches, and peak memory stays under 3× the final graph size

### Requirement: Image size limit
The system SHALL skip images larger than 20MB with a warning log message. The base64 encoding (1.33× expansion) means effective limit is ~15MB original file size.

#### Scenario: Large image skipped with warning
- **WHEN** a 25MB PNG file is encountered
- **THEN** the system logs "Image too large (25MB > 15MB limit): photo.png — creating stub node" and produces a stub node without calling the vision API