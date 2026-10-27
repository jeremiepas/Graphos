# AVI-1452 — Bounded node ids/labels; bounded signature; linear `graph.json`

**Issue:** `f3b7fdae-108f-4979-a373-8e91800314c0` (parent `9f8d33ef-1cd2-4533-84b7-8f4e6325998c` = AVI-1450, done). High priority. Scope = task 3.3 in `openspec/changes/usable-graph-for-code-review/tasks.md`.

> **Authoritative check criteria (task 3.3):** tests assert (a) the id layout `<dirHash>_<stem>_<kind>:<identifier>`, (b) the `#L<line>` overload suffix on in-file collisions, (c) run-to-run determinism, (d) **absence of source text in ids**, and (e) a **512-byte per-node bound on a 5,000-line fixture**. Verify with `cabal test`.

## 1. Root cause

`Convert.makeNode` (lines 424–440) feeds the **whole, untruncated** declaration text into the id and label, and never emits a signature:

```haskell
makeNode filePath node = Node
  { nodeId           = makeNodeId filePath (tsNodeUntruncatedLabel node)  -- full decl -> unbounded id
  , nodeLabel        = fromText (tsNodeLabel node)                        -- full decl -> unbounded label
  , nodeSignature    = Nothing                                            -- no declaration text anywhere
  , nodePresentBits  = bitNodeLineStart .|. bitNodeLineEnd .|. bitNodeKind
  }
```

`tsNodeUntruncatedLabel node` returns the full normalized declaration (`const API_URL = 'x'`, a multi-line function body, …). `makeNodeId` embeds it verbatim. The same untruncated text then flows through `makeNodeId` in `tsNodeToGraphEdges` (contains edges, lines 317–319) and into `applyTaggingSources` (Core.hs line 410). Result: `graph.json` is super-linear in node count (>512 MB on large inputs).

The correct helpers already exist but are **not wired in**:
- `nodeKeyId :: SourceFile -> Text -> Text -> Int -> Int -> NodeId` (line 628) → `<dirHash>_<stem>_<kind>:<ident>` with `#L<line>` when `rank > 0`.
- `mkSourceFile :: FilePath -> SourceFile` (line 594).
- `truncateText :: Int -> Text -> Text` and `defaultTruncationBudget` (Core.hs) — the named truncation budget.
- `bitNodeSignature` + `nodeSignature :: Maybe ShortText` on `Node` (Node.hs) — the bounded signature field.

## 2. Scope boundary

**In scope (this issue):**
- `Convert.makeNode`: bounded `nodeId` + `nodeLabel`, bounded `nodeSignature`, `bitNodeSignature` present bit.
- `tsNodeToGraphEdges` contains-edge endpoints use the same bounded id scheme (not the old path scheme).
- A deterministic in-file collision pass that appends `#L<line>` and rewires contains edges.
- `extractionConfigFingerprint` bump in Core.hs for cache invalidation.
- Tests: id layout, `#L` suffix, determinism, no-source-text-in-id, 512-byte/nod bound.

**Out of scope (leave alone):**
- The uncommitted `feature/AVI-1447-calls-edge-emission` diff (call-edge helpers). Do NOT commit, switch, or rename that branch.
- `applyTaggingSources` multi-source remapping (Core.hs line 410): it stays bounded because `makeNodeId` is unchanged; route a follow-up separately if you want it to adopt the scheme.
- Identifier *extraction* (parsing `const API_URL` → `API_URL`). `ident` = the normalized node text bounded by the budget; this is the AVI-984/973 contract.

## 3. `makeNode` rewrite (`Convert.hs` lines 424–440)

Replace the body with a helper that builds the **seed** id (rank 0, no suffix). The `#L<line>` suffix is applied later by the collision pass (it needs sibling context this function lacks):

```haskell
makeNode :: FilePath -> TSNodeInfo -> Node
makeNode filePath node =
    let sf    = mkSourceFile filePath
        kind  = fromText (T.pack $ tsTypeToKind (tsnType node))
        line  = tsnStartRow node + 1
        ident = truncateText defaultTruncationBudget (tsNodeUntruncatedLabel node)
        sig   = truncateText defaultTruncationBudget (tsNodeUntruncatedLabel node)
    in Node
         { nodeId           = nodeKeyId sf kind ident line 0   -- seed id, no suffix
         , nodeLabel        = fromText ident                     -- identifier (bounded)
         , nodeFileType     = CodeFile
         , nodeSourceFile   = fromText (T.pack filePath)
         , nodeSource       = Nothing
         , nodeLineStart    = Just line
         , nodeLineEnd      = Just (tsnEndRow node + 1)
         , nodeKind         = Just kind
         , nodeSignature    = Just (fromText sig)                -- bounded declaration text
         , nodePresentBits  = bitNodeLineStart .|. bitNodeLineEnd .|. bitNodeKind .|. bitNodeSignature
         , nodeCommunityId  = Nothing
         , nodeDegree       = Nothing
         , nodeIsBridge     = Nothing
         , nodeExtra        = Nothing
         }
```

Notes:
- `nodeLabel` becomes the **identifier** (`ident`), not the raw text. For import/export nodes `tsNodeLabel` already elides to ≤ budget, so a downstream import-specifier display is preserved; the label here is the bounded identifier.
- `ident` and `sig` are truncated to `defaultTruncationBudget` so both id and signature are bounded → per-node size bounded → linear file. Keep the same budget for both so id and label stay consistent.

## 4. Contains-edge consistency + collision pass

The contains edges in `tsNodeToGraphEdges` (lines 314–325) currently compute their endpoints with the old path scheme:

```haskell
edgeId        = EdgeId (makeNodeId filePath p <> "->" <> makeNodeId filePath myLabel <> ":contains")
edgeSource    = makeNodeId filePath p
edgeTarget    = makeNodeId filePath myLabel
```

After §3, node ids are the `<dirHash>_<stem>_<kind>:<ident>` seed ids, so these edges would dangle. Two fixes are required:

### 4a. Thread the parent's seed id down the recursion

`tsNodeToGraphEdges` is threaded `parentLabel :: Maybe Text`. Add a parallel `parentId :: Maybe NodeId` so the contains edge references the **exact** seed id of the parent node (and never recomputes it). There is a single internal caller, `tsNodesToExtraction` (line 288), so changing this exported signature is low-risk (ConvertSpec only calls `tsNodesToExtraction`).

```haskell
tsNodeToGraphEdges
  :: Granularity -> FilePath -> CallsCtx -> Maybe NodeId -> TSNodeInfo
  -> ([Edge], [Node])
tsNodeToGraphEdges gran filePath ctx parentId node =
    let myId   = nodeKeyId (mkSourceFile filePath)
                           (fromText (T.pack $ tsTypeToKind (tsnType node)))
                           (truncateText defaultTruncationBudget (tsNodeUntruncatedLabel node))
                           (tsnStartRow node + 1) 0
        myEdges = case (parentId, isDef) of
          (Just p, True) ->
            [ Edge
              { edgeId        = EdgeId (p <> "->" <> myId <> ":contains")
              , edgeSource    = p
              , edgeTarget    = myId
              , edgeRelation  = Contains
              , edgeConfidence = Confidence 1.0
              , edgeWeight    = 1.0
              , edgeExtra     = Nothing
              }
            ]
          _ -> []
        ...
        -- when recursing into children, thread myId (not myLabel):
        foldr (\child (es, ns) ->
                 let (e, n) = tsNodeToGraphEdges gran filePath childCtx (Just myId) child
                 in (e ++ es, n ++ ns)) ([], []) (tsnChildren node)
```

### 4b. Deterministic in-file collision pass

Two nodes in the same file can share a seed id (e.g. two functions `getX`). Append `#L<ownLine>` for every occurrence after the first, ordered deterministically by `(lineStart, seedId)`. Run it once at the end of `tsNodesToExtraction`, after `extractionFromLists`:

```haskell
applyCollisionRanks :: Extraction -> Extraction
applyCollisionRanks ex =
    let byFile = Map.fromListWith (++)
              [ (toText (nodeSourceFile n), [(lineKey n, seedId n, n)]) | n <- Map.elems (extractionNodes ex) ]
        assign :: [(Int, NodeId, Node)] -> [(NodeId, NodeId, Node)]
        assign  = zipWith (\i (l, seed, n) ->
                  let fin = if i > 0 then seed <> "#L" <> T.pack (show l) else seed
                  in (seed, fin, n)) [0..]
        pairs  = concat [ assign (sortWith (\(l,_,_) -> (l,)) lst) | (_, lst) <- Map.toAscList byFile ]
        remap  = Map.fromList [ (seed, fin) | (seed, fin, _) <- pairs ]
        newNodes = Map.fromList [ (fin, n { nodeId = fin }) | (_, fin, n) <- pairs ]
        newEdges = Map.map (remapContains remap) (extractionEdges ex)
    in ex { extractionNodes = newNodes, extractionEdges = newEdges }
  where
    lineKey n = fromMaybe 0 (nodeLineStart n)
    seedId n  = nodeId n                                   -- makeNode emitted the seed id
    remapContains remap e =
      case edgeRelation e of
        Contains ->
          let s = Map.findWithDefault (edgeSource e) (edgeSource e) remap
              t = Map.findWithDefault (edgeTarget e) (edgeTarget e) remap
          in e { edgeSource = s, edgeTarget = t,
                 edgeId   = EdgeId (s <> "->" <> t <> ":contains") }
        _ -> e
```

Wire it in `tsNodesToExtraction`:

```haskell
tsNodesToExtraction gran filePath nodes =
    let results = map (\node ->
            let ctx = collectCallsCtx filePath node
            in (tsNodeToGraphNodes gran filePath node, tsNodeToGraphEdges gran filePath ctx Nothing node)) nodes
        graphNodes = concatMap fst results
        extraNodes = concatMap (snd . snd) results
        graphEdges = concatMap (fst . snd) results
        rawEx = applyCollisionRanks (extractionFromLists (graphNodes ++ extraNodes) graphEdges)
    in rawEx
```

Why `Map.toAscList byFile` + `sortWith (lineKey, seedId)`: the key is the file, and `Map.toAscList` gives a deterministic file ordering; within a file, sorting by `(lineStart, seedId)` makes the rank assignment run-to-run stable (criterion c). Only occurrences after the first get `#L<line>`, and because colliding nodes sit on distinct lines the suffixes are unique (criterion a/b).

`remapContains` touches only `Contains` edges: external/import stub nodes use the untouched `makeNodeId` path scheme and are not in `byFile`'s Group-3 files, so their ids are never remapped (they are not Group-3 nodes).

## 5. Cache invalidation (`Core.hs` `extractionConfigFingerprint`, lines 535–551)

The extraction cache is keyed by this fingerprint. The id scheme is an extraction-affecting value, so under **INV-CACHE-SOUND** (AVI-521 §1.2) the fingerprint MUST change to invalidate stale (old-format) cache entries:

```haskell
extractionConfigFingerprint config = T.pack . unwords $
  concat
    [ [ "granularity=" ++ show (gcGranularity gcfg)
      , "cliGranularity=" ++ maybe "none" show (cfgGranularity config)
      , "pdf=" ++ pdfLevelName (gcPdfExtraction gcfg)
      , "nodeIdentity=av1452-bounded-ids"   -- AVI-1452: id scheme changed
      ]
    , [ "extractor." ++ ext ++ "=" ++ show (ecMode ec)
            ++ ";granularity=" ++ maybe "auto" show (ecGranularity ec)
      | (ext, ec) <- Map.toAscList (gcExtractors gcfg)
    ]
  ]
```

Adding `"nodeIdentity=av1452-bounded-ids"` changes the key, so all previously cached extractions are re-extracted with the new bounded ids.

## 6. Tests

### 6a. Update `tests/.../TreeSitter/ConvertSpec.hs`
`tsNodeToGraphEdges` now takes `Maybe NodeId` as its 4th argument (was `Maybe Text`) — update its (internal) call site in `tsNodesToExtraction` per §4a. The ConvertSpec call site already goes through `tsNodesToExtraction`, so no direct signature change is needed there, but verify it compiles.

Existing assertions on `nodeLabel` for **short** inputs (< budget) mostly survive, because `ident` is the normalized text truncated to budget and the fixtures are short: `"return user;"`, `"const API_URL = 'x'"`, `"import { db } from './db'"` are unchanged. Assertions that WILL need updating:
- Any test asserting an `edgeTarget`/`edgeSource` equals the old `makeNodeId` path form — contains edges now carry `<dirHash>_<stem>_<kind>:<ident>` seed ids (and suffixed ids on collisions).
- The `function granularity` "emits no contains-edges into function bodies" assertion (line 102–105): still `False`, but it now inspects suffixed ids.

### 6b. New assertions (authoritative, per task 3.3 check criteria)
Add a describe block covering the bounded-id contract. Use the existing `tsNode` helper.

```haskell
describe "bounded node ids and labels (AVI-1452)" $ do
  it "uses the <dirHash>_<stem>_<kind>:<ident> layout, not full source text" $ do
    let ex = tsNodesToExtraction GranularityFunction "src/service.ts" [tsFixture]
        ids = Map.keysMap (extractionNodes ex)
    -- no id embeds a space or '=' (signs of embedded declaration bodies)
    all (\i -> not (T.any (== '=') i) && not (T.any (isSpace ' ') i)
                && "Service" `T.isInfixOf` i) (Map.keys ids) `shouldBe` True
    -- the file stem appears as the second segment
    any ("service" `T.isInfixOf`) (Map.keys ids) `shouldBe` True

  it "keeps nodeLabel = identifier (no whole declaration)" $ do
    let ex = tsNodesToExtraction GranularityFunction "src/service.ts" [tsFixture]
        labels = map (T.unpack . toText . nodeLabel) (Map.elems (extractionNodes ex))
    labels `shouldSatisfy` elem "getUser"
    labels `shouldSatisfy` notElem ("const API_URL = 'x'" `T.isInfixOf`)  -- trimmed to identifier
```

Add a **separate** test module `tests/Graphos/Infrastructure/Extract/TreeSitter/ConvertSpecBounded.hs` (or a new describe) with:

```haskell
  it "appends #L<line> only to in-file collisions, deterministically" $ ...
  -- Build a fixture with two same-named definitions in one file, assert the
  -- first keeps the bare seed id and the second carries #L<ownLine>.

  it "is run-to-run deterministic (stable id regardless of node ordering)" $ ...
  -- Shuffle children order and assert identical id set.
```

### 6c. Regression: per-node size bound (task 3.3 criterion e)
Add a test against a synthetic 5,000-node / 5,000-line extraction and assert a per-node bound:

```haskell
  it "keeps graph.json per-node size bounded (<= 512 bytes) on a 5,000-node graph" $ do
    let big = tsNodesToExtraction GranularityFile "big/mod.ts"
                          [ 5000-line fixture producing many nodes ]
        json = renderGraphJson big                       -- via exportGraphWithLabels / incremental writer
        perNode = byteSize json / nodeCount json
    perNode `shouldSatisfy` (<= 512)
```

The 512-byte bound is the hard acceptance criterion. If the default 200-char budget makes a node exceed it (id ≤200 + label ≤200 + signature ≤200 + JSON key overhead ≈ 80), lower `defaultTruncationBudget` until the test passes, or make the signature budget smaller than the id/label budget. The test is the gate — do not loosen it.

### 6d. Do not break JGF contract tests
`tests/.../JGFSpec.hs` builds **hand-written short-id fixtures** (`"a"`, `"b"`) and round-trips through the writer/reader. Because the JGF node serialization format is unchanged (Node ToJSON/FromJSON untouched), these keep passing. The change only affects *how ids are produced during extraction*, not the serialization schema. Verify with `cabal test` at the end.

## 7. Verification (run by the task-bound implementer)

```bash
cabal test            # all above tests green
# sanity: build a large graph and confirm linear growth
cabal run graphos extract -- <large-repo> out/
python3 - <<'PY'
import json, glob, os
sizes = [os.path.getsize(p) for p in glob.glob("out/**/*.json", recursive=True)]
print("graph.json bytes:", max(sizes))
PY
```

Expected: ids contain no spaces/`=`, labels are short identifiers, `graph.json` size grows linearly with node count, and the per-node bound holds.


- Set `bitNodeSignature` so `computePresentBits`/FromJSON round-trip preserves the signature.
