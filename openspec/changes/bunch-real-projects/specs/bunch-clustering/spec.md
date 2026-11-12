## MODIFIED Requirements

### Requirement: File-level module dependency graph

The system SHALL fold a graph of any granularity to a module dependency graph
whose nodes are the distinct `nodeSourceFile` values and whose undirected
edges join two files when an edge whose relation is in the Bunch relation
allowlist links a node of one file to a node of the other. The allowlist SHALL
be `clustering.bunch.relations`, default `imports`, `calls`, `references`,
`extends`, `implements`. It SHALL NEVER admit `contains`, `documents`,
`depends_on`, `inferred` or any relation produced by edge inference. Self-pairs
SHALL be dropped. The weight of a file pair SHALL be 1, or the sum of the
folded edges' `edgeWeight` when `--bunch-weighted` is set. Files SHALL be
indexed in ascending path order before any random draw. (PRD §5.1)

#### Scenario: symbol edges fold to one file pair
- **WHEN** functions in `a.ts` call functions in `b.ts` three times and `b.ts` imports `a.ts`
- **THEN** the MDG has one edge `{a.ts, b.ts}` of weight 1, or weight 4 when weighted

#### Scenario: contains-only graph
- **WHEN** the only edges are `contains` edges from files to their symbols
- **THEN** the MDG has the files as nodes and no edges

#### Scenario: node order does not matter
- **WHEN** the same graph is folded from two different node orders
- **THEN** the file indices and adjacency are identical

#### Scenario: inferred and documentation edges are ignored
- **WHEN** `a.ts` and `b.ts` are linked only by a `depends_on` edge from edge inference and `README.md` is linked to `a.ts` by a `documents` edge
- **THEN** the MDG has no edge `{a.ts, b.ts}` and no edge involving `README.md`

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

## ADDED Requirements

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
- **THEN** at most 1 % of the modules are single files and MQ is at least 370 (the partitioned search without refinement gave 1,579 singletons and MQ 149)
