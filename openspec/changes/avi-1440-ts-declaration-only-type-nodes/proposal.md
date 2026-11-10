## Why

At `function` (and `file`) granularity, the TreeSitter extractor emits a `kind: "Type"`
node for **every** TypeScript `type_identifier` node in the tree. `type_identifier`
covers both declared types and bare type *references*, so source such as:

```ts
export interface User {
  createdAt: Date;      // `Date` is a reference, not a declaration
}

export type UserId = number;   // `number` is a reference, not a declaration
export type Name = string;
```

currently produces the graph nodes `Interface(User)`, `Type(Date)`, `Type(number)`,
`Type(string)` — three phantom nodes that name types **referenced** by the code but are
never themselves declared. Those reference nodes are orphans with no real declaration
to point at, and they pollute every downstream view (degree inflation, noise on
`Type`-labelled queries, and dead edges once references are wired).

## What Changes

- The extractor SHALL treat a bare `type_identifier` node as a **reference**, not a
  declaration, and MUST NOT emit a `kind: "Type"` node for it. Only **declared** types
  yield a `Type` node.
- Concretely, `type_identifier` is removed from the API-surface type whitelist
  (`apiSurfaceTypes` in `Convert.hs`) and its `type_identifier -> Type` clause in
  `tsTypeToKind` is dropped (replaced by a documenting comment listing the declaration
  kinds that DO yield `Type`).
- References are **not** emitted as edges in this change (out of scope); they are simply
  dropped, consistent with the "references are not nodes" treatment of the surrounding
  extraction.
- Behaviour in other languages is unchanged: every other whitelist entry and every
  other `tsTypeToKind` clause maps exactly as before.

## Requirements

See the delta spec `extraction-granularity` below.

## Impact

- **Infrastructure**: `Graphos.Infrastructure.Extract.TreeSitter.Convert` — two targeted
  lines removed from the whitelist and one `tsTypeToKind` clause repurposed into a
  comment; no signature change, no behaviour outside the TS type whitelist.
- **Tests**: new regression in `GrammarsSpec` (real parse of the vendored
  `example/ts-lsp-test/src/types.ts` fixture) asserting declared `interface`/`enum`/
  `type_alias` survive and reference labels (`Date`, `number`, `string`) are absent, with
  exactly one `Type` node.
- **Downstream**: the graph no longer carries phantom reference `Type` nodes; degrees and
  `Type`-labelled views are clean at the source.
- Low risk: whitelist narrowing only; other languages and other node kinds are untouched.
