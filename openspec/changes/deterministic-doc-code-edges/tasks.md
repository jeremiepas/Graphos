## 1. Domain

- [ ] 1.1 Add `documents` edge relation and confidence tagging to the edge model
- [ ] 1.2 Ensure `documents` is classified into the semantic (non-ambiguous) edge set
- [ ] 1.3 Tests: edge relation serialization and semantic-set membership

## 2. Co-location pass

- [ ] 2.1 Implement pure function linking a doc node to code in same/descendant directory
- [ ] 2.2 Guard against cross-subtree links (top-level README stays in its subtree)
- [ ] 2.3 Tests: sibling link created, unrelated-dir link not created

## 3. Symbol-mention pass

- [ ] 3.1 Reuse the symbol/definition index to resolve identifiers mentioned in doc text
- [ ] 3.2 Match whole-word identifiers length ≥ 4 with exactly one definition; skip ambiguous
- [ ] 3.3 Tests: defined-symbol mention links; common-word and ambiguous cases skipped

## 4. Path-reference pass

- [ ] 4.1 Implement pure function resolving explicit repo-relative file paths in doc text to code nodes by `source_file`
- [ ] 4.2 Match path-like tokens that contain a `/` separator and a known source extension; normalize a leading `./`; require the path to resolve to an existing node (skip dangling paths)
- [ ] 4.3 Link across directory subtrees (unlike co-location); tag as `documents` high confidence
- [ ] 4.4 Tests: cited path links doc→file nodes across subtrees; non-existent path and bare filename (no separator) skipped

## 5. Pipeline integration

- [ ] 5.1 Wire the three passes (co-location, symbol-mention, path-reference) into the Infer stage after edge inference
- [ ] 5.2 Keep similarity `inferred` edges but ensure separation from `documents`
- [ ] 5.3 Make thresholds (min identifier length, path extensions) configurable

## 6. Verification

- [ ] 6.1 `cabal build --flag dev` with `-Werror`
- [ ] 6.2 `cabal test` green including linking suites
- [ ] 6.3 Query a documented component with `edges=semantic`; confirm doc AND code nodes returned
