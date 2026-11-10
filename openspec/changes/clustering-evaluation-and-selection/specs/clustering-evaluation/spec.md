## ADDED Requirements

### Requirement: Directory ground truth

The system SHALL derive a reference partition of file nodes in which each file
belongs to the group named by its directory path truncated to depth `d` below
the corpus root (default `d = 2`, overridden by `--truth-depth`). When given
graphs from several refs, the system SHALL keep only files present in every
graph and SHALL put two files in the same group if and only if they share a
group in every ref. When `--truth FILE` is given, the system SHALL use that
JSON file→group map instead. (PRD §5.1 via community-detection)

#### Scenario: depth-2 grouping
- **WHEN** the files are `src/a/x.ts`, `src/a/y.ts`, `src/b/z.ts` and `d = 2`
- **THEN** the ground truth has two groups: `{src/a/x.ts, src/a/y.ts}` and `{src/b/z.ts}`

#### Scenario: multi-ref intersection splits a moved grouping
- **WHEN** in ref 1 `x` and `y` share a group and in ref 2 they do not
- **THEN** the multi-ref ground truth puts `x` and `y` in different groups

#### Scenario: file missing from one ref is dropped
- **WHEN** a file is present in ref 2 only
- **THEN** it is absent from the multi-ref ground truth and counted in the dropped-node total

### Requirement: MoJo distance and MoJoFM

The system SHALL compute `mno(A,B)` as the MoJo distance of Wen & Tzerpos
(2004) between two partitions restricted to their common nodes, and
`MoJoFM(A,B) = (1 − mno(A,B) / max(mno(∀A,B))) × 100` with B the reference,
returning 100 when the maximum is 0. The result SHALL report the number of
nodes dropped from each side.

#### Scenario: identical partitions
- **WHEN** A equals B
- **THEN** `mno(A,B) = 0` and `MoJoFM(A,B) = 100`

#### Scenario: closed-form maximum matches brute force
- **WHEN** B is any partition of n ≤ 6 nodes
- **THEN** the maximum of `mno(A,B)` over all partitions A equals the closed form used by MoJoFM

#### Scenario: range
- **WHEN** MoJoFM is computed for any two partitions of the same node set
- **THEN** the result lies in [0, 100]

### Requirement: Partition stability

The system SHALL compute the stability of two partitions from two builds as
MoJoFM in both directions on their common nodes.

#### Scenario: unchanged build is stable
- **WHEN** two builds of the same ref produce the same partition
- **THEN** both stability values are 100

### Requirement: clustereval command

The CLI SHALL provide `graphos clustereval score|compare|sweep` taking absolute
or relative `--graph` paths, `--truth-depth`, `--truth`, repeated `--ref-graph`
for multi-ref truth, and `--json`. `score` SHALL fold the stored node
communities to files by majority community before scoring.

#### Scenario: score emits JSON
- **WHEN** `graphos clustereval score --graph g.json --json` runs on a clustered graph
- **THEN** stdout is one JSON object with `mojofm`, `truth_groups`, `clusters`, `dropped`

#### Scenario: compare two builds
- **WHEN** `graphos clustereval compare --graph a.json --graph b.json --json` runs
- **THEN** the output contains `stability_ab` and `stability_ba`
