## Context

`Graphos.CLI.Parser` builds every option with `optparse-applicative`. Three
styles coexist today:

1. `option auto` — the reader is the derived `Read` instance. Used for all five
   enum options and ~30 numeric options. The help string is an unrelated literal.
2. Hand-written `eitherReader` with an inline `case` — `granularityReader`,
   `detectModeReader`, `edgeModeReader`, `parseAlgorithmName`. These accept the
   documented lowercase tokens, but the token list still appears twice: once in
   the `case`, once in the `help` literal.
3. Validating readers — `boundedIntReader`, `fractionReader`, `annealingReader`,
   `heapSizeReader`. These already produce flag-named, range-naming errors.

Style 1 is where the vocabulary diverged; style 2 is where it can diverge next
(nothing links the `case` arms to the help string); style 3 is the pattern worth
generalising. The architecture rule applies: Domain stays IO-free, so token
tables for Domain enums are pure values that the CLI layer imports.

## Goals / Non-Goals

**Goals:**
- One table per enum is the single source of truth for reader, metavar and help.
- Lowercase tokens are the documented vocabulary, matching `PushMode`'s JSON
  instances, `graphos.yaml`, the workflow docs and spec `12-neo4j-push`.
- Every invocation accepted today keeps working.
- Parse-time rejection of out-of-range numerics, with the range in the message.
- Drift is caught by `cabal test`, not by review attention.

**Non-Goals:**
- Changing any default value, or any pipeline/export behaviour.
- Removing the constructor-spelling aliases (deferred to a future major).
- Wiring the inert `neo4j.pushMode` / `memgraph.pushMode` config fields.
- Reworking `--bunch-sa`, `--max-heap`, `--random-graph` — structured readers
  with their own grammar and already-good errors.
- Shell completion for enum values (natural follow-up, not required here).

## Decisions

### D1 — Derive reader, metavar and help from one table

```
data EnumFlag a = EnumFlag
  { efName    :: String          -- "--edge-density", used in errors
  , efMetavar :: String          -- "DENSITY"
  , efTokens  :: [(String, a)]   -- primary tokens, in help order
  , efAliases :: [(String, a)]   -- accepted, never printed
  }
```

`enumOption :: EnumFlag a -> Mod OptionFields a` emits `option (enumReader f) (metavar (efMetavar f) <> …)`, and `enumHelp :: EnumFlag a -> String` renders `sparse|normal|dense|maximum` from `efTokens`. A help string can no longer disagree with the reader because neither is written by hand.

*Alternatives considered.* (a) Keep `option auto` and rewrite the help strings to
`Sparse|Normal|Dense|Maximum` — cheapest, but contradicts spec `12-neo4j-push`,
the README, the workflow docs and `PushMode`'s own JSON vocabulary, and exposes
Haskell constructor names as a user-facing contract. Rejected. (b) Hand-write an
`eitherReader` per flag, as `granularityReader` does — fixes the five flags but
reproduces the exact duplication that caused the drift. Rejected. (c) Derive
tokens mechanically from `Enum`/`Bounded` plus a `show`-lowercasing function —
`MemgraphFull → "full"` and `SubgraphPush → "subgraph"` are not mechanical, and
aliases have nowhere to live. Rejected.

### D2 — Tables for Domain enums live beside the types

`edgeDensityTokens`, `neo4jPushModeTokens` and `memgraphPushModeTokens` are
exported from `Graphos.Domain.Types.Pipeline`; the `EnumFlag` record and the
reader combinators live in a new `Graphos.CLI.Readers`. Pure data in Domain, no
IO, CLI depends on Domain and not the reverse. This also leaves the vocabulary
reachable from the config loader and report rendering when the adjacent config
finding is addressed, without a second table.

*Alternative:* put the tables in `Graphos.CLI.Parser`. Rejected — it would force
any future config-side use of the same vocabulary to duplicate it.

### D3 — Case-insensitive matching, constructor spellings as silent aliases

Lookup lowercases both sides. `efAliases` carries `Sparse`, `FullPush`,
`MemgraphSubgraph` and friends. Consequences: every invocation that parses today
still parses (the accepted set only grows); `--help` stays short because aliases
are never printed; and the run that was re-issued with `--edge-density Sparse`
needs no further change.

*Alternative:* hard cutover to lowercase only. Rejected — it breaks existing
scripts and CI invocations for no benefit this change needs. A deprecation
warning on alias use was also considered; `ReadM` cannot emit one without
threading IO through the parser, so aliases stay silent.

### D4 — Error shape

`--edge-density: unknown value 'sparce'. Expected one of: sparse, normal, dense, maximum`
— option name first, offending value quoted, vocabulary last. Matches the existing
`boundedIntReader` style (`--seed out of range [0,∞]: -1`) so all option errors read
alike. optparse-applicative's exit code for a reader failure (1) is unchanged.

### D5 — Numeric bounds

Numeric options move from `auto` to `boundedIntReader` / a new
`boundedDoubleReader`, with the bound chosen from the flag's documented meaning.
Where `0` is a documented sentinel it stays legal.

| Bound | Options |
| --- | --- |
| `1..65535` | `--port`, `--metrics` |
| `>= 1` | `--threads`, `--lsp-concurrency`, `--max-leiden-iterations`, `--min-comm-size`, `--timeout`, `--otel-shutdown-timeout`, `--minified-threshold`, `--max-sampled-sources`, `--exact-betweenness-node-cap`, `--neo4j-subgraph-size`, `--memgraph-subgraph-size`, `--subgraph-size` |
| `>= 0` | `--budget`, `--label-width`, `--depth`, `--boundary-hops`, `--max-nodes` (0 = unlimited), `--max-label-chars` (0 = default) |
| `> 0.0` | `--resolution` |

This is an intentional narrowing: `--threads -1` and `--port 99999` are accepted
today and fail later or behave oddly. Metavars (`N`, `PORT`, `SECONDS`, `FLOAT`)
replace the bare `ARG` in usage output.

### D6 — Anti-drift is enforced by a test, not a convention

`Graphos.CLI.Parser` exports an `enumFlagRegistry` listing every `EnumFlag` in
the CLI. `ParserSpec` walks it and asserts, per entry: each primary token parses;
each primary token appears in the rendered `--help` body for that option; the
rendered usage line carries the metavar; an unknown token fails with a message
containing every primary token. A new enum option added without a table entry
fails the registry-coverage assertion.

## Risks / Trade-offs

- **Two vocabularies stay alive.** Aliases mean `Sparse` and `sparse` both work
  indefinitely. Mitigated by printing only primary tokens in help and docs;
  removal is a deliberate future decision, not drift.
- **~35 option sites are edited mechanically**, so a transposed default is the
  realistic failure mode. Mitigated by the existing `parsePipeline []` default
  assertions in `ParserSpec` (extended to cover every option's default) and by
  `-Wall -Werror` under `--flag dev`.
- **Narrowing numerics can reject an invocation that parses today** — only ones
  that are already meaningless (`--threads 0`, negative sizes). The bound table
  above is the complete list of newly-rejected inputs.
- **Registry duplication risk**: an option could be built without going through
  `enumOption`. The coverage assertion in D6 catches the common case; a reviewer
  still has to notice a hand-rolled `eitherReader` added later.

## Verification

- `cabal build --flag dev` — clean under `-Wall -Wcompat -Wincomplete-uni-patterns -Werror`.
- `cabal test` — `Graphos.CLI.ParserSpec` registry, bounds, alias and default cases green.
- Manual: `graphos --edge-density sparse .`, `graphos --neo4j-push-mode full .`,
  `graphos --edge-density Sparse .` (alias), `graphos --edge-density sparce .`
  (error names the vocabulary), `graphos --help` (usage shows `--edge-density DENSITY`).
