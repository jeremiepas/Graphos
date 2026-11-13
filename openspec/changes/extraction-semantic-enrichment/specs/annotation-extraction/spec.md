# Annotation Extraction

## ADDED Requirements

### Requirement: Annotations are captured as structured node metadata

For every grammar with an annotation construct (java `annotation` /
`marker_annotation`, python and typescript `decorator`, c-sharp
`attribute_list`, rust `attribute_item`), the tree-sitter extractor SHALL
record the annotations attached to a definition on that definition node as
`nodeExtra.annotations = [{name, args}]`, where `args` is the normalized
source text of the argument list (absent for marker annotations), bounded to
16 annotations per node and 120 characters per `args`. PHP is excluded: the
grammar version this extractor is pinned to (`tree-sitter-php` 0.5.0.2) has no
attribute-syntax node of any kind.

#### Scenario: JAX-RS method annotations are captured

- **WHEN** a Java method is annotated `@GET @Path("/{id}")`
- **THEN** its Method node carries
  `annotations = [{name:"GET"}, {name:"Path", args:"(\"/{id}\")"}]`

### Requirement: Resolvable annotations emit a references edge

When an annotation's name resolves through the file's import bindings, the
extractor SHALL emit one `references` edge (confidence 1.0) from the annotated
definition node to the import's target node. Unresolvable annotation names
emit no edge.

#### Scenario: @GET links the method to the imported symbol

- **WHEN** a Java file imports `javax.ws.rs.GET` and a method is annotated
  `@GET`
- **THEN** a `references` edge exists from the Method node to the
  `javax.ws.rs.GET` node

### Requirement: Well-known annotations are distilled to queryable keys

A data-driven distillation table SHALL map well-known annotation families to
flat keys on the annotated node's `nodeExtra`: `route` (JAX-RS @Path, Spring
@RequestMapping/@GetMapping…, flask/fastapi route decorators, NestJS
@Get/@Post), `httpVerb` (@GET/@POST/… and the mapping shortcuts), `cron` and
`fixedDelay` (@Scheduled), `topics` (@KafkaListener, @RabbitListener),
`configKeys` (@Value `${…}` keys). Unknown annotations are captured but not
distilled.

#### Scenario: route and verb are readable without the source

- **WHEN** a Java class is annotated `@Path("/v1/clients")` and its method
  `@GET @Path("/{id}")`
- **THEN** the Method node carries `httpVerb = "GET"` and `route = "/{id}"`,
  and the Class node carries `route = "/v1/clients"`

#### Scenario: @Value yields a config key

- **WHEN** a field is annotated `@Value("${fineract.tenant.host}")`
- **THEN** the Field node carries `configKeys = ["fineract.tenant.host"]`
