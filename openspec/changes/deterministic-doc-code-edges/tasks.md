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

## 4. Pipeline integration

- [ ] 4.1 Wire both passes into the Infer stage after edge inference
- [ ] 4.2 Keep similarity `inferred` edges but ensure separation from `documents`
- [ ] 4.3 Make thresholds (min identifier length) configurable

## 5. Verification

- [ ] 5.1 `cabal build --flag dev` with `-Werror`
- [ ] 5.2 `cabal test` green including linking suites
- [ ] 5.3 Query a documented component with `edges=semantic`; confirm doc AND code nodes returned
