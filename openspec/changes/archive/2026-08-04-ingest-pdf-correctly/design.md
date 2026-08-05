## Context

PDF files are detected as `PaperFiles` by the pipeline but never extracted — `UseCase.Extract.extractAll` processes `CodeFiles`, `DocFiles`, `OfficeFiles`, and `ImageFiles`, but `PaperFiles` are silently dropped. The `ingest` command's URL path for PDFs writes only a stub `[PDF content - to be fetched]`. The test file `Maison-Rustique-T5.pdf` is a 55K-line 19th-century French agricultural encyclopedia with rich hierarchical structure: TITRE → CHAPITRE → SECTION → § → lettered items.

Current architecture layers affected:

```
Domain (pure)
  ├── Types.Node          → PaperFile already exists ✓
  ├── Types.Pipeline      → PaperFiles already exists ✓
  └── Config.Extraction    → fecPaper = [".pdf"] already exists ✓

UseCase (pure orchestration)
  ├── Extract.hs          → MISSING: PaperFiles routing ✗
  ├── Ingest.hs           → MISSING: real PDF extraction ✗
  └── Port.ExtractionPort → MISSING: epExtractPdfFile field ✗

Infrastructure (IO)
  └── Extract/            → MISSING: Pdf.hs module ✗
```

## Goals / Non-Goals

**Goals:**
- PDF files produce a rich knowledge graph with hierarchical section nodes, paragraph nodes, and Contains/References edges
- Three granularity levels (Small/Medium/Large-paragraph) matching existing `Granularity` type
- TOC pages detected and skipped to avoid duplicate nodes
- Missing `pdftotext` produces warning + stub node, pipeline continues
- URL PDF ingestion downloads and extracts instead of creating stubs
- Maison-Rustique-T5.pdf produces communities that cluster thematically (e.g., all "Engrais" sections together)

**Non-Goals:**
- OCR for scanned/image PDFs (future iteration)
- Data table extraction with row/column structure (future iteration)
- Page-level nodes (can be added later)
- PDF form field extraction
- Encryption/password-protected PDF support

## Decisions

### D1: Use `pdftotext` CLI as extraction backend

**Decision**: Call `pdftotext` (from poppler-utils) as a subprocess to extract text from PDFs.

**Alternatives considered**:
| Alternative | Pros | Cons |
|---|---|---|
| **A. pdftotext CLI** (chosen) | Simple, robust, handles all PDF types, no Haskell FFI, matches existing pattern (LSP servers are also CLI) | External dependency, no structured output (plain text only) |
| B. Haskell PDF library (e.g., poppler via Hackage) | Pure Haskell, no external binary | Fragile bindings, limited maintenance, complex FFI |
| C. Pandoc CLI | Handles many formats, structured output | Heavy dependency for just PDF, different formatting |
| D. Apache Tika | Rich extraction (metadata, tables) | JVM dependency, overkill for text extraction |

**Rationale**: `pdftotext` is the lightest tool that does the job well. It matches Graphos's existing pattern of calling external CLI tools (LSP servers). The Nix shell already includes system dependencies. Plain text output is sufficient when combined with our heuristic section parser.

### D2: Section detection via regex heuristics on pdftotext output

**Decision**: Parse `pdftotext` output using regex patterns to detect hierarchical sections.

**Patterns detected** (generalized from Maison-Rustique):
| Pattern | Level | Examples |
|---|---|---|
| ALL CAPS line ≥ 4 words | 1 (Title) | `MAISON RUSTIQUE`, `HORTICULTURE` |
| Numbered sections `1.`, `1.1` | Level by dot count | `1. Introduction`, `2.3.4 Detail` |
| Roman numeral headers `CHAP.`, `TITRE` | 2 | `CHAP. Ier. Terrains` |
| `seCT.` prefix | 3 | `seCT. Ire. Engrais` |
| `§` prefix | 4 | `§ Ier. Engrais végétal` |
| Lettered items `A.`, `B.` after § | 5 | `A Bêche commune` |
| Blank line separation | Paragraph | Text blocks between headers |

**Alternatives considered**:
| Alternative | Pros | Cons |
|---|---|---|
| **A. Regex heuristics** (chosen) | Works for structured PDFs, no extra deps, fast | May miss non-standard formatting |
| B. pdftotext -layout + column detection | Preserves visual layout | Harder to parse, layout varies wildly |
| C. pdftohtml → structured HTML | Preserves headings, tables | Additional parsing, formatting noise |
| D. PDF metadata (bookmarks/outlines) | Semantic, not heuristic | Most PDFs lack bookmarks |

**Rationale**: Regex heuristics cover the common cases well (academic papers, books, structured docs). The patterns are ordered by specificity so more specific patterns match first. Unknown formatting falls back to paragraph-level extraction. The heuristic parser lives in a separate pure module (`Domain.PdfStructure`) for testability.

### D3: Three granularity levels mapped to existing type

**Decision**: PDF granularity maps to the existing `Granularity` type:

| Granularity | PDF Level | Nodes Created | Example |
|---|---|---|---|
| `GranularityFile` (Small) | File + top-level titles | ~5-10 nodes | File node + each TITRE |
| `GranularityFunction` (Medium) | File + sections | ~20-50 nodes | File + CHAP + seCT nodes |
| `GranularityFine` (Large/default) | Paragraph-level | ~100-500 nodes | All levels + paragraphs |

**Alternatives considered**:
| Alternative | Pros | Cons |
|---|---|---|
| **A. Reuse existing Granularity** (chosen) | Consistent with rest of pipeline, no new types | Semantics differ slightly (function ≠ section) |
| B. New PdfGranularity type | More precise semantics | Adds type complexity, diverges from pipeline convention |
| C. Always paragraph-level | Maximum detail | Too many nodes for large PDFs (55K lines → thousands) |

**Rationale**: Reusing `Granularity` keeps the pipeline consistent. The CLI flag `--granularity` already exists. Default is Fine (paragraph-level) because that's what produces the richest communities.

### D4: TOC detection and skip

**Decision**: Detect TOC pages by identifying patterns: lines with dot-leaders (`....`), page number references (`ib.`, numeric page refs at line end), and sequential section listings. Skip these pages entirely.

**Heuristic criteria** (all must be met for a page to be classified as TOC):
- ≥ 60% of non-empty lines contain dot-leaders (`...` 3+ dots)
- Page number references at line ends (`ib.`, or `\d+` at end)
- No paragraph-length text blocks (> 100 chars without dots)

**Alternatives considered**:
| Alternative | Pros | Cons |
|---|---|---|
| **A. Heuristic TOC skip** (chosen) | No external deps, works for common cases | May misclassify some pages |
| B. PDF bookmark comparison | Definitive | Most PDFs lack bookmarks |
| C. No TOC handling | Simplest | Produces duplicate nodes for every structured PDF |
| D. Dedup by title matching | Catches any duplicate | Complex, false positives when titles repeat in body |

**Rationale**: The heuristic is simple and effective for well-structured PDFs like Maison-Rustique. If a page is misclassified, the worst case is losing some navigation nodes — never losing content.

### D5: Architecture — new module in Infrastructure, new port field, UseCase routing

**Decision**: Clean architecture pattern:

```
Domain (pure)
  └── PdfStructure.hs     — PDF structure types + pure parser

UseCase (pure orchestration)
  ├── Extract.hs           — add PaperFiles processing path
  ├── Ingest.hs            — fix PdfUrl to download + extract
  └── Port/ExtractionPort.hs — add epExtractPdfFile field

Infrastructure (IO)
  └── Extract/Pdf.hs       — pdftotext CLI call + text parsing
```

**Data flow**:
```
.pdf file
  → Infrastructure.Extract.Pdf.extractPdfFile
    → call pdftotext (or warn + stub if missing)
    → get Text output
    → Domain.PdfStructure.parsePdfStructure (pure)
      → detect + skip TOC pages
      → detect sections (regex heuristics)
      → build PdfStructure (hierarchy of sections + paragraphs)
    → convert PdfStructure → Extraction (nodes + edges)
  → UseCase.Extract.extractAll routes PaperFiles through epExtractPdfFile
```

**Alternatives considered**:
| Alternative | Pros | Cons |
|---|---|---|
| **A. Separate pure parser + IO caller** (chosen) | Testable, clean architecture | More modules |
| B. All in Infrastructure | Simpler | Untestable parser logic |
| C. All in UseCase | Violates IO boundary | UseCase shouldn't call pdftotext |

**Rationale**: Separating the pure parser (`Domain.PdfStructure`) from the IO caller (`Infrastructure.Extract.Pdf`) follows the project's clean architecture pattern. The parser is independently testable with Hspec + QuickCheck. The IO module only calls `pdftotext` and delegates parsing to the pure module.

### D6: URL PDF ingestion — download then extract

**Decision**: When `ingest` encounters a `PdfUrl`, download the PDF to a temp file, then route through `extractPdfFile`. If download fails, create a stub node.

**Alternatives considered**:
| Alternative | Pros | Cons |
|---|---|---|
| **A. Download + extract** (chosen) | Full extraction, consistent with file ingest | Requires HTTP download |
| B. Download + stub | Simple | No better than current behavior |
| C. Defer to pipeline | Batch processing | Breaks single-file ingest UX |

**Rationale**: Full extraction is the right behavior. Graphos already has HTTP capabilities (used for URL fetching in the existing `ingest` function). The download uses the existing `validateUrl` and a simple HTTP GET.

## Risks / Trade-offs

| Risk | Mitigation |
|---|---|
| `pdftotext` not installed | Log warning, create stub node, pipeline continues. Add to shell.nix. |
| PDF with no structure (plain text) | Falls back to paragraph-level extraction (blank-line separation) |
| TOC false positive (body text classified as TOC) | Conservative heuristic (60% threshold); worst case = lost nav nodes, not lost content |
| Very large PDFs (Maison-Rustique is 55K lines) | Paragraph-level granularity produces many nodes but community detection handles it; Small/Medium available |
| French/special characters (é, è, ê, à, ç) | `pdftotext` outputs UTF-8; Haskell `Text` handles Unicode natively |
| Regex pattern fragility across PDF types | Patterns ordered by specificity; unknown formatting → paragraph fallback; patterns configurable in graphos.yaml |

## Verification Strategy (Check)

1. **Unit tests** (`cabal test`): Pure parser in `Domain.PdfStructure` tested with Hspec + QuickCheck
   - Test TOC detection heuristic on known TOC and non-TOC text
   - Test section detection regex patterns on sample French PDF text
   - Test granularity levels produce correct node counts
   - Test paragraph splitting on blank lines

2. **Integration test**: `graphos ingest Maison-Rustique-T5.pdf`
   - Verify >0 nodes extracted with `PaperFile` type
   - Verify section hierarchy: TITRE → CHAP → seCT → § → items
   - Verify Contains edges: File → TITRE, TITRE → CHAP, CHAP → seCT
   - Verify communities cluster thematically (e.g., "Engrais" sections together)
   - Verify bridge nodes connect across communities
   - Verify `--granularity file` produces fewer nodes than `--granularity fine`

3. **Error path test**: Missing `pdftotext`
   - Verify warning logged
   - Verify stub node created
   - Verify pipeline continues without crash

4. **URL ingestion test**: Verify PdfUrl downloads and extracts (mock HTTP)

5. **Build verification**: `cabal build` and `cabal test` both pass

## Iteration & Rollback (Act)

**If Check fails (extraction produces wrong structure)**:
- Adjust regex patterns in `Domain.PdfStructure`
- Tune TOC detection threshold
- Add new PDF test fixtures to corpus

**If Check fails (pdftotext not available)**:
- Already handled: stub node + warning
- Follow-up: Consider bundling a static pdftotext binary or using a Haskell PDF library as fallback

**If Check fails (too many nodes from paragraph-level)**:
- Adjust default granularity to Medium for PDFs > threshold
- Add `--pdf-granularity` CLI override

**Rollback**: The change is additive — new port field, new module, new routing. Removing `epExtractPdfFile` and the PaperFiles path in `extractAll` reverts to current behavior (PDFs silently dropped). No data migration needed.

**Standardization for next cycle**: If PDF extraction works well, extend to:
- OCR for scanned PDFs (via Tesseract)
- Table extraction (structured row/column nodes)
- PDF metadata extraction (author, title, dates as node properties)
- Page-level nodes with cross-references