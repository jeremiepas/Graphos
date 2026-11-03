## MODIFIED Requirements

### Requirement: TypeScript `Type` nodes come only from declarations, not references

In addition to the `Function-level extraction stops at function bodies` requirement
above, at `function` (and `file`) granularity the TreeSitter extractor SHALL emit a
`kind: "Type"` node for a **declared** TypeScript type only — a `type_declaration`
(`type X = …`), a `type_alias_declaration` (`interface X { … }`), or an
`enum_declaration` — and SHALL **not** emit a `Type` node for a bare `type_identifier`
that names a type *referenced* by the code (a field annotation such as `role: UserRole`,
or the `number`/`string` in `type UserId = number`). A `type_identifier` is a reference,
not a declaration, and produces no node. (PRD §3.2 extract; AVI-1440.)

#### Scenario: Only declared types survive for a mixed TS file

- **WHEN** a `.ts` file declares an `interface User` with a `createdAt: Date` field, an
  `enum UserRole`, and a `type UserId = number` alias, and is extracted at `function`
  granularity
- **THEN** nodes exist for `Interface(User)`, `Enum(UserRole)`, and `Type(UserId)`, and
  no node exists for the reference labels `Date`, `number`, or `string`, verifiable by
  `cabal test`

#### Scenario: A reference does not create a phantom `Type` node

- **WHEN** a `.ts` file only *references* types (e.g. `role: UserRole; createdAt: Date`)
  with no declaration of them and is extracted at `function` granularity
- **THEN** no `Type` node is produced for `UserRole`, `Date`, or any other referenced
  type, verifiable by `cabal test`
