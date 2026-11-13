# Notable String Extraction

## ADDED Requirements

### Requirement: Context-gated notable strings become canonical nodes

The tree-sitter extractor SHALL emit nodes for string literals only when a
context gate matches — never for bare literals:

- **Env gate**: the literal is the argument of an environment-read form of the
  file's grammar (`System.getenv`, `os.environ[…]`/`os.getenv`,
  `process.env.<X>`, `std::env::var`, `ENV[…]`, `getenv`, `$_ENV[…]`) ⇒ node
  id `env:<NAME>`, kind `Env`.
- **Config gate**: a `${…}` placeholder inside an annotation argument or a
  dotted key in `@Value` / `@ConfigurationProperties` ⇒ node id
  `config:<key>`, kind `Config`.
- **Topic gate**: the first string argument of a listener/producer annotation
  (@KafkaListener, @RabbitListener) ⇒ node id `topic:<name>`, kind `Topic`.

Canonical ids carry no directory hash: the same name from any file resolves to
the same node. Each gated literal also emits one `references` edge
(confidence 0.9) from the enclosing definition node.

#### Scenario: getenv at function granularity

- **WHEN** a Java method body contains `System.getenv("CLIENTS_TABLE")` and
  extraction runs at function granularity (opaque bodies)
- **THEN** a node `env:CLIENTS_TABLE` of kind `Env` exists with a `references`
  edge from the enclosing Method node

#### Scenario: the same env var converges across files

- **WHEN** two files in different directories both read `CLIENTS_TABLE`
- **THEN** exactly one `env:CLIENTS_TABLE` node exists, with a `references`
  edge from each reader

### Requirement: Notable-string noise is bounded

Per file, at most 32 notable strings SHALL be emitted after deduplication;
names outside 1–80 characters are ignored; literals dropped by the cap are
logged at debug level. Extraction never fails on pathological string content.

#### Scenario: over-cap file degrades gracefully

- **WHEN** a file's gates match 50 distinct literals
- **THEN** 32 nodes are emitted, the remainder are dropped with a debug log,
  and extraction succeeds
