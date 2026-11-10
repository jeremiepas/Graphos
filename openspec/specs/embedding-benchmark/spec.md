# embedding-benchmark Specification

## Purpose

Reproducible local benchmark of embedding throughput per model, server, and concurrency on a node-shaped corpus, so model choices and pipeline defaults are grounded in recorded measurements rather than assumptions.

## Requirements

### Requirement: Node-shaped benchmark corpus

The benchmark SHALL run against a corpus whose texts are shaped like real pipeline embed inputs — a label concatenated with a source-file path, drawn from or synthesized to match the label and source-file length distribution of a production graph — and SHALL report texts per second as its headline metric.

#### Scenario: Corpus matches pipeline text shape
- **WHEN** the benchmark runs
- **THEN** every measured input is a `<label> <sourcefile>` string, and no single text exceeds the model's token limit after preparation

#### Scenario: Headline metric reported
- **WHEN** a benchmark run completes
- **THEN** the result includes texts/s, wall-clock duration, text count, model name, server base URL, batch size, and concurrency level

### Requirement: Recorded results gate defaults

The benchmark results SHALL be recorded in the change directory (`BENCHMARK.md`) including date, hardware description (CPU, cores, RAM, GPU presence), server and model configuration, and the measured throughputs. Changes to the default model or default concurrency SHALL cite a recorded benchmark run.

#### Scenario: Adoption decision cites measurement
- **WHEN** a default model or default concurrency changes
- **THEN** the decision references a dated row in `BENCHMARK.md` measured on the target-class hardware

#### Scenario: Model comparison is reproducible
- **WHEN** the benchmark is re-run with the same model, server, and concurrency on the same machine
- **THEN** the measured throughput is within a factor of 2 of the previously recorded value

### Requirement: Concurrency scaling measurement

The benchmark SHALL measure throughput at concurrency levels 1, 2, 4, and 8, holding the model and batch size fixed, so the default concurrency can be pinned to the measured plateau rather than guessed.

#### Scenario: Concurrency sweep recorded
- **WHEN** the benchmark completes
- **THEN** `BENCHMARK.md` reports texts/s at each of the four concurrency levels, and the pinned default concurrency corresponds to the lowest level that reaches the measured throughput plateau

### Requirement: Oversized-input regression probe

The benchmark SHALL include a probe that submits at least one text longer than the model's token limit (before preparation), verifying that the pipeline's token-fit preparation prevents rejection and that per-input failure isolation holds when the server rejects an input.

#### Scenario: Oversized text does not lose siblings' vectors
- **WHEN** the benchmark submits a batch containing one text longer than the model's limit alongside normal texts
- **THEN** the oversized text's vector is either produced via chunked preparation or withheld alone, and every normal text in the batch still receives its vector