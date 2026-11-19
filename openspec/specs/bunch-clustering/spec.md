# bunch-clustering Specification

## Purpose
TBD - created by archiving change bunch-search-based-modularization. Update Purpose after archive.

## Requirements

### Requirement: File-level module dependency graph

The system SHALL fold a graph of any granularity to a module dependency graph
whose nodes are the system's code files, and whose undirected
edges join two files when an edge whose relation is in the Bunch relation
allowlist links a node of one file to a node of the other. The allowlist SHALL
be `clustering.bunch.relations`, default `imports`, `calls`, `references`,
`extends`, `implements`. A code file is a `nodeSourceFile` with at least one
`CodeFile` node that is not an import-target placeholder (kind `External`, or
kind `Module` with no line span). Packages, builtins, paths outside the
corpus, and documentation, image or office files SHALL NOT be MDG nodes, and
edges into them add no pair. After the search, the nodes of such a file SHALL
join the module holding most of their neighbours over the allowlisted
relations and `documents`, ties to the lowest module id; a file with no such
neighbour SHALL form a module of its own. It SHALL NEVER admit `contains`, `documents`,
`depends_on`, `inferred` or any relation produced by edge inference. Self-pairs
SHALL be dropped. The weight of a file pair SHALL be 1, or the sum of the
folded edges' `edgeWeight` when `--bunch-weighted` is set. Files SHALL be
indexed in ascending path order before any random draw. (PRD §5.1)

#### Scenario: symbol edges fold to one file pair
- **WHEN** functions in `a.ts` call functions in `b.ts` three times and `b.ts` imports `a.ts`
- **THEN** the MDG has one edge `{a.ts, b.ts}` of weight 1, or weight 4 when weighted

#### Scenario: packages and out-of-corpus targets are not modules
- **WHEN** `a.ts` and `b.ts` import each other and both import the package `zod`, and `a.ts` imports `src/domain/x.ts`, which is not in the corpus
- **THEN** the MDG files are `a.ts` and `b.ts` with one pair, and the `zod` and `src/domain/x.ts` placeholder nodes join the module of their importers

#### Scenario: documentation is not a module
- **WHEN** `docs/a.md` documents `a1.ts` and `a2.ts`, and edge inference later adds `references` edges from code to `specs/x.md` headers
- **THEN** neither Markdown file is an MDG file; `docs/a.md` joins the module of `a1.ts`

#### Scenario: measured subtree baseline
- **WHEN** solario-core `14d86be28` `src/cli` (161 TS files) is clustered with Bunch
- **THEN** the MDG has 161 files, not the 351 counted when each package and each `src/domain` target was a file, and no file with a dependency edge inside `src/cli` is a module of its own

#### Scenario: contains-only graph
- **WHEN** the only edges are `contains` edges from files to their symbols
- **THEN** the MDG has the files as nodes and no edges

#### Scenario: node order does not matter
- **WHEN** the same graph is folded from two different node orders
- **THEN** the file indices and adjacency are identical

#### Scenario: inferred and documentation edges are ignored
- **WHEN** `a.ts` and `b.ts` are linked only by a `depends_on` edge from edge inference and `README.md` is linked to `a.ts` by a `documents` edge
- **THEN** the MDG has no edge `{a.ts, b.ts}` and no edge involving `README.md`

### Requirement: Hill-climbing search over partitions

With `--cluster-algorithm bunch` the system SHALL start from a random
partition (cluster count drawn uniformly from 1..n, each file assigned
uniformly) and SHALL repeatedly replace it by a candidate with strictly higher
MQ until no candidate improves it or `--bunch-max-iterations` (default 10,000)
is reached. Candidates SHALL be the neighbouring partitions (one file moved to
another cluster or to a new singleton) followed by the building-block
partitions (one new cluster of exactly two files joined by an MDG edge),
enumerated in canonical order: files ascending, target clusters ascending,
new singleton, then building blocks by ascending file pair. The threshold
`--bunch-threshold η` (0–100, default 0) SHALL require at least ⌈η · |C|⌉ of
the |C| candidates to be examined per iteration before the best examined
improvement is taken; η = 0 SHALL take the first improvement and η = 100
SHALL take the best of all candidates. (PRD §5.1)

#### Scenario: two cliques
- **WHEN** the MDG is two disjoint 5-cliques joined by one edge
- **THEN** for every seed in 0..31 the search converges to the two cliques with MQ = 2·(2·10 / (2·10 + 1))

#### Scenario: MQ never decreases without annealing
- **WHEN** the search runs with annealing disabled
- **THEN** the MQ of the current partition is non-decreasing across iterations

#### Scenario: threshold changes work, not result
- **WHEN** the two-clique fixture is searched with η ∈ {0, 50, 100} from the same seed
- **THEN** the three runs reach the same MQ, and each run reports its number of MQ evaluations and iterations

#### Scenario: building block opens a cluster
- **WHEN** the current partition has every file in one cluster and the MDG has two disjoint edges
- **THEN** a building-block candidate exists for each edge and at least one has higher MQ

### Requirement: Simulated annealing option

The system SHALL, when `--bunch-sa T0,alpha` is set (defaults `T0 = 100`,
`alpha = 0.99`), accept a non-improving candidate with probability
`exp(ΔMQ / T)` for `ΔMQ < 0`, SHALL set `T ← alpha · T` after every
probability draw, and SHALL count the accepted non-improving candidates. With
annealing disabled the count SHALL be 0.

#### Scenario: annealing off
- **WHEN** the search runs without `--bunch-sa`
- **THEN** zero non-improving candidates are accepted

#### Scenario: annealing on
- **WHEN** the two-clique fixture is searched with `--bunch-sa 100,0.99`
- **THEN** the search terminates and reports the number of accepted non-improving candidates

### Requirement: Population and determinism

The system SHALL run `--bunch-population p` (default 8) independent searches
from seeds `s, s+1, …, s+p−1` where `s` is `--seed` (default 0), SHALL keep
the partition with the highest MQ (lowest seed on ties), and SHALL relabel its
clusters 0..k−1 by ascending smallest member file. For a fixed graph,
configuration and seed the resulting `CommunityMap` SHALL be identical across
runs and across node orderings of the input graph.

#### Scenario: reproducible
- **WHEN** the same graph is clustered twice with `--cluster-algorithm bunch --seed 7`
- **THEN** the two `CommunityMap`s are equal

#### Scenario: permutation invariant
- **WHEN** the same graph is supplied with its nodes in two different orders
- **THEN** every node receives the same community id in both runs

### Requirement: File cap and symbol push-back

The system SHALL run Bunch on any MDG up to `--bunch-max-files` (default
50,000) files. Up to `--bunch-max-files-per-run` (default 500) files it SHALL
run one search. Above that it SHALL run the partitioned search of the
*Partitioned search* requirement. Above `--bunch-max-files` it SHALL skip Bunch
and run Leiden, logging the file count and the ceiling. When Bunch runs, every
symbol node SHALL receive its file's cluster. The community-size cap post-pass
(`--max-community-fraction`) SHALL NOT apply to a Bunch partition, and neither
SHALL `mergeSmallCommunities`.

#### Scenario: cap exceeded
- **WHEN** a graph folds to 60,000 files and `--bunch-max-files` is 50,000
- **THEN** the partition equals the Leiden partition of the same build and the log names the skip with 60,000 and 50,000

#### Scenario: symbols follow their file
- **WHEN** Bunch places `a.ts` in cluster 3
- **THEN** every node whose `nodeSourceFile` is `a.ts` has community 3 in `graph.json`

#### Scenario: no size cap on small graphs
- **WHEN** Bunch clusters a 29-file MDG (nestjs-realworld-example-app, 65 import pairs) with the default `--max-community-fraction 0.05`
- **THEN** no module is split by the cap, the partition has fewer modules than files, and MQ is greater than 0 (it was 0, one file per module, before this change)

### Requirement: Partitioned search

When the MDG has more files than `--bunch-max-files-per-run`, the system SHALL
split the files into directory sub-trees of at most that many files and run the
Bunch search on each sub-tree's induced MDG:

- A group above the maximum is split by its next path component.
- Sibling groups smaller than one tenth of the maximum are pooled into one
  sub-tree.
- A group whose files share a single directory is cut into consecutive chunks
  in ascending path order.
- Sub-trees are processed in ascending order of their smallest file path, each
  with the configured seed, population, threshold and annealing.
- Module ids from successive sub-trees are offset so they never collide.

The merged partition SHALL then go through the *Global refinement* pass. The
report SHALL state the number of sub-tree runs and their sizes.

#### Scenario: large graph is clustered, not skipped
- **WHEN** a graph folds to 3,929 files with the default limits
- **THEN** Bunch runs on sub-trees of at most 500 files, no Leiden fallback is logged, and the report lists the sub-tree runs

#### Scenario: deterministic split
- **WHEN** the same MDG is clustered twice with the same seed, or with its nodes in a different order
- **THEN** the sub-trees, their order and the final `CommunityMap` are identical

### Requirement: Global refinement

After every Bunch search, the system SHALL run a refinement pass over the whole
MDG, whether the search was single or partitioned. The pass visits files in
ascending path order. For each file it considers moving the file to each module
holding one of its MDG neighbours, evaluates the MQ change with all MDG edges
(including edges between sub-trees), and applies the move with the largest gain
when that gain exceeds 1e-12. Passes SHALL repeat until a pass applies no move,
or until 50 passes. Every applied move SHALL strictly increase MQ, and the
result SHALL remain a partition of the MDG files. The report SHALL state the
number of moves and passes.

#### Scenario: cross-sub-tree singletons are absorbed
- **WHEN** a partitioned search leaves a file alone because all its imports point into other sub-trees
- **THEN** after refinement the file belongs to a module holding one of its import neighbours, if that raises MQ

#### Scenario: refinement never lowers MQ
- **WHEN** the refinement pass runs on any partition
- **THEN** the MQ after the pass is greater than or equal to the MQ before it

#### Scenario: single search is already refined
- **WHEN** an MDG small enough for one search is clustered
- **THEN** the refinement pass applies zero moves or only moves that raise MQ, and the partition remains deterministic for the seed

#### Scenario: measured large-repository baseline
- **WHEN** the pc26 build (3,929 files, 17,292 import pairs) is clustered with the default limits
- **THEN** at most 1 % of the modules are single files that have a dependency edge (a file with none cannot move under MQ), no Leiden fallback is logged, and the MQ in `GRAPH_REPORT.md` (scored over the Bunch MDG relations between code files) is at least 330 (the partitioned search without refinement gave 1,579 singletons)
