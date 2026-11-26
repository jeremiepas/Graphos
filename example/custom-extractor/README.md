# Custom command extractor example

Runs a small external extractor for Perl files instead of Graphos' built-in
Perl tree-sitter binding.

```bash
cd example/custom-extractor
graphos .
```

(`extract.py` is resolved relative to the working directory, so run Graphos
from this directory.)

`graphos.yaml` binds `.pl` to:

```yaml
".pl":
  mode: command
  command: ["python3", "extract.py", "{file}"]
  timeout: 10
```

`extract.py` reads one file and prints graph.json-shaped nodes and edges on
stdout: a module node, one node per `sub`, an `imports` node per `use`, plus
`contains`/`imports` edges. Graphos validates the records, prefixes ids with a
per-file bucket, fills `source_file`, and merges the extraction.

Try breaking it: change `command` to a missing executable and rerun. Graphos
records a per-file `ExtractionError`, stub-extracts `hello.pl`, and completes
the build instead of aborting. See
[Workflow 01](../../docs/workflows/01-full-pipeline.md#custom-extractors) for
the full contract.
