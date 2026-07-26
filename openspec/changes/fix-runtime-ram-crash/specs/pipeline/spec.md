## MODIFIED Requirements

### Requirement: RTS profiling CLI flags

The CLI SHALL accept `--rts-profile` and `--max-heap SIZE` flags that configure the GHC runtime for memory profiling and heap size limits. `--rts-profile` enables GC statistics output and heap profiling. `--max-heap` sets a maximum heap size, causing graceful failure if exceeded.

Previously: No CLI flags for RTS profiling. Users had to know to append `+RTS -s -h` after `--`.

- **Plan**: Make memory debugging and heap limiting discoverable via first-class CLI options.
- **Do**: Parse `--rts-profile` and `--max-heap` in `Main.hs`, set the corresponding RTS options before program start (or pass them via `+RTS` in the executable wrapper).
- **Check**: `graphos . --rts-profile` produces GC statistics on stderr. `graphos . --max-heap 4G` fails gracefully when heap exceeds 4GB.
- **Act**: If `--max-heap` causes premature failure on legitimate workloads, document recommended heap sizes per codebase size.

#### Scenario: RTS profiling enables GC statistics
- **WHEN** `graphos . --rts-profile` is run
- **THEN** GHC runtime GC statistics are printed to stderr upon exit
- **AND** a heap profile file (`graphos.hp`) is generated in the output directory

#### Scenario: Max heap limits memory
- **WHEN** `graphos . --max-heap 4G` is run on a codebase that would exceed 4GB of heap
- **THEN** the process exits with a clear error message indicating the heap limit was exceeded
- **AND** the error message suggests increasing `--max-heap` or reducing the codebase size

#### Scenario: Both flags can be combined
- **WHEN** `graphos . --rts-profile --max-heap 8G` is run
- **THEN** both heap profiling and the 8GB limit are active simultaneously