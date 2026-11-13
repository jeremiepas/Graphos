{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Extract.TreeSitter.ConvertSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Short (toText, fromText)

import Data.Aeson (object, (.=))
import Graphos.Domain.Config (Granularity(..))
import Graphos.Domain.Types.Graph (extractionNodes, extractionEdges)
import Graphos.Domain.Types.Edge (edgeRelation, edgeSource, edgeTarget)
import Graphos.Domain.Types (Relation(..))
import Graphos.Domain.Types.Node (nodeLabel, nodeId, nodeSignature, nodeExtra, nodeKind)
import Graphos.Infrastructure.Extract.TreeSitter.Convert
import Graphos.Infrastructure.Extract.TreeSitter.Core
  ( TSNodeInfo(..), SourceFile(..), mkSourceFile, defaultTruncationBudget )

-- | Build a named AST node.
tsNode :: String -> T.Text -> [TSNodeInfo] -> TSNodeInfo
tsNode ty txt children = TSNodeInfo
  { tsnType = ty
  , tsnFieldName = Nothing
  , tsnText = txt
  , tsnStartRow = 0, tsnStartCol = 0, tsnEndRow = 0, tsnEndCol = 0
  , tsnIsNamed = True
  , tsnChildren = children
  }

-- | Tag a node with its tree-sitter grammar field name, as if it had been
-- parsed as a named child (e.g. the `name` field of a `method_declaration`).
withField :: String -> TSNodeInfo -> TSNodeInfo
withField field n = n { tsnFieldName = Just field }

-- | A named identifier-class child (as tree-sitter emits for a declared name).
ident :: T.Text -> TSNodeInfo
ident t = tsNode "identifier" t []


-- | Function declaration with an explicit declared name.
func :: T.Text -> TSNodeInfo
func name = tsNode "function_declaration" (T.pack ("function " ++ T.unpack name)) [ident name]

-- | Class declaration with an explicit declared name.
cls :: T.Text -> TSNodeInfo
cls name = tsNode "class_declaration" (T.pack ("class " ++ T.unpack name)) [ident name]

-- | Class declaration with explicit children (e.g. member methods).
clsWith :: T.Text -> [TSNodeInfo] -> TSNodeInfo
clsWith name children = tsNode "class_declaration" (T.pack ("class " ++ T.unpack name)) (ident name : children)

-- | Java-shaped method: a `type_identifier` return type positioned *before*
-- the name, as real Java grammar emits it, with the name tagged with field
-- `name` (as `node-types.json` confirms `method_declaration` exposes).
javaMethod :: T.Text -> T.Text -> TSNodeInfo
javaMethod retTy name =
  tsNode "method_declaration" (T.pack (T.unpack retTy ++ " " ++ T.unpack name ++ "()"))
    [ tsNode "type_identifier" retTy []
    , withField "name" (ident name)
    ]

-- | The same shape as 'javaMethod' but with no field tags at all — simulates
-- a grammar (or a hand-built fixture) that carries no field metadata, so the
-- fallback identifier-type scan is what has to get this right.
javaMethodNoFields :: T.Text -> T.Text -> TSNodeInfo
javaMethodNoFields retTy name =
  tsNode "method_declaration" (T.pack (T.unpack retTy ++ " " ++ T.unpack name ++ "()"))
    [ tsNode "type_identifier" retTy []
    , ident name
    ]

-- | Rust const item with an explicit declared name.
constant :: T.Text -> TSNodeInfo
constant name = tsNode "const_item" (T.pack ("const " ++ T.unpack name)) [ident name]

-- | Import declaration carrying a quoted specifier in its text.
importDecl :: T.Text -> TSNodeInfo
importDecl specStr = tsNode "import_statement" (T.pack ("import { x } from " ++ T.unpack specStr)) []

-- | Export wrapper around an inner function declaration.
exportFunc :: T.Text -> TSNodeInfo
exportFunc name =
  tsNode "export_statement" (T.pack ("export " ++ T.unpack name)) [func name]

-- | Decorated Python function (decorated_definition wrapper).
decoratedFn :: T.Text -> TSNodeInfo
decoratedFn name =
  tsNode "decorated_definition" (T.pack ("@marker\n" ++ T.unpack name)) [func name]

sf :: SourceFile
sf = mkSourceFile "src/service.ts" ""

spec :: Spec
spec = do
  describe "3.1 identifier extraction" $ do
    it "function identifier is the declared name" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [func "getUser"]
          nodes = Map.elems (extractionNodes ex)
      length nodes `shouldBe` 1
      nodeLabel (case nodes of (x:_) -> x; [] -> error "empty nodes") `shouldBe` fromText "getUser"
      nodeId (case nodes of (x:_) -> x; [] -> error "empty nodes") `shouldBe` makeNodeId sf "Function" "getUser"

    it "class / type identifier is the declared name" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [cls "Service"]
          n = case Map.elems (extractionNodes ex) of (x:_) -> x; [] -> error "empty extractionNodes"
      nodeLabel n `shouldBe` fromText "Service"
      nodeId n `shouldBe` makeNodeId sf "Class" "Service"

    it "constant identifier is the declared name" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [constant "MAX"]
          n = case Map.elems (extractionNodes ex) of (x:_) -> x; [] -> error "empty extractionNodes"
      nodeLabel n `shouldBe` fromText "MAX"
      nodeId n `shouldBe` makeNodeId sf "Constant" "MAX"

    it "import identifier is the resolved specifier" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [importDecl "'./db'"]
          is = filter (\m -> nodeKind m == Just (fromText "Import")) (Map.elems (extractionNodes ex))
          n = case is of (x:_) -> x; [] -> case Map.elems (extractionNodes ex) of (y:_) -> y; [] -> error "empty extractionNodes"
      nodeLabel n `shouldBe` fromText "./db"

    it "module identifier is the file stem" $ do
      let ex = tsNodesToExtraction sf GranularityFile defaultTruncationBudget
             [tsNode "program" "module" [func "x"]]
          n = case Map.elems (extractionNodes ex) of (y:_) -> y; [] -> error "empty extractionNodes"
      nodeLabel n `shouldBe` fromText "service.ts"
      nodeId n `shouldBe` makeNodeId sf "Module" "service.ts"
      nodeSignature n `shouldBe` Nothing

  describe "3.2 wrapper suppression" $ do
    it "export_statement yields one node flagged exported" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [exportFunc "f"]
          nodes = Map.elems (extractionNodes ex)
      length nodes `shouldBe` 1
      let n = case nodes of (x:_) -> x; [] -> error "empty nodes"
      nodeLabel n `shouldBe` fromText "f"
      nodeKind n `shouldBe` Just (fromText "Function")
      nodeExtra n `shouldBe` Just (object ["exported" .= (True :: Bool)])

    it "decorated_definition yields one node flagged exported" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [decoratedFn "g"]
          nodes = Map.elems (extractionNodes ex)
      length nodes `shouldBe` 1
      nodeLabel (case nodes of (x:_) -> x; [] -> error "empty nodes") `shouldBe` fromText "g"
      nodeExtra (case nodes of (x:_) -> x; [] -> error "empty nodes") `shouldBe` Just (object ["exported" .= (True :: Bool)])

  describe "3.3 new id layout + collision suffix" $ do
    it "id layout is <dirHash>_<stem>_<kind>:<identifier>" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [func "h"]
          n = case Map.elems (extractionNodes ex) of (x:_) -> x; [] -> error "empty extractionNodes"
          parts = T.splitOn "_" (nodeId n)
      length parts `shouldBe` 3
      T.all (\c -> c >= '0' && c <= '9') (parts !! 0) `shouldBe` True   -- dirHash numeric prefix
      parts !! 1 `shouldBe` "service"          -- stem
      parts !! 2 `shouldBe` "Function:h"       -- kind:identifier

    it "no source text and no ellipsis embedded in ids" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [func "getUser"]
          id1 = nodeId (case Map.elems (extractionNodes ex) of (x:_) -> x; [] -> error "empty extractionNodes")
      T.isInfixOf "..." id1 `shouldBe` False
      T.isInfixOf "getUser(" id1 `shouldBe` False

    it "overloads stay distinct: only the later occurrence gets #L<line>" $ do
      let fixture = tsNode "program" "module"
             [ func "handle" `withLine` 10
             , func "handle" `withLine` 40 ]
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [fixture]
          ids = sortIds (map nodeId (filter (\n -> nodeKind n == Just (fromText "Function")) (Map.elems (extractionNodes ex))))
      ids `shouldBe` [ makeNodeId sf "Function" "handle"
                     , makeNodeId sf "Function" "handle" <> "#L40" ]

    it "overloaded siblings BOTH keep their contains edge from the class (regression: Map.fromList last-wins collapse)" $ do
      let overload1 = javaMethod "ClientData" "helper" `withLine` 10
          overload2 = javaMethod "ClientData" "helper" `withLine` 20
          classNode = clsWith "ClientApi" [overload1, overload2]
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [classNode]
          classId = makeNodeId sf "Class" "ClientApi"
          methodIds = sortIds [ makeNodeId sf "Method" "helper"
                               , makeNodeId sf "Method" "helper" <> "#L20" ]
          containsFromClass =
            sortIds [ edgeTarget e
                    | e <- Map.elems (extractionEdges ex)
                    , edgeRelation e == Contains, edgeSource e == classId ]
      containsFromClass `shouldBe` methodIds

  describe "3.4 field-aware declaration names (extraction-semantic-enrichment)" $ do
    it "a method's name field wins over a preceding type_identifier return type" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget
             [javaMethod "ClientData" "retrieveClient"]
          n = case Map.elems (extractionNodes ex) of (x:_) -> x; [] -> error "empty extractionNodes"
      nodeLabel n `shouldBe` fromText "retrieveClient"
      nodeKind n `shouldBe` Just (fromText "Method")

    it "without field metadata, a plain identifier still wins over type_identifier (fallback)" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget
             [javaMethodNoFields "ClientData" "retrieveClient"]
          n = case Map.elems (extractionNodes ex) of (x:_) -> x; [] -> error "empty extractionNodes"
      nodeLabel n `shouldBe` fromText "retrieveClient"

    it "a bare type_identifier reference emits no node (AVI-1440)" $ do
      -- GranularityFine so the walk actually descends into the function body
      -- (at GranularityFunction a function's body is opaque and this would
      -- pass trivially without exercising the fix).
      let fnWithParamType = tsNode "function_declaration" "function f(x: ClientData)"
                [ ident "f"
                , tsNode "formal_parameters" "(x: ClientData)"
                    [tsNode "type_identifier" "ClientData" []] ]
          ex = tsNodesToExtraction sf GranularityFine defaultTruncationBudget
             [tsNode "program" "module" [fnWithParamType]]
          kinds = [ nodeKind n | n <- Map.elems (extractionNodes ex) ]
      Just (fromText "type_identifier") `shouldNotSatisfy` (`elem` kinds)

    it "C/C++ function name resolves through a nested declarator field chain" $ do
      let declarator =
            tsNode "function_declarator" "retrieveClient(long id)"
              [ withField "declarator" (ident "retrieveClient")
              , tsNode "parameter_list" "(long id)" [] ]
          fn = tsNode "function_definition" "ClientData retrieveClient(long id) {}"
                 [ tsNode "type_identifier" "ClientData" []
                 , withField "declarator" declarator
                 , tsNode "compound_statement" "{}" [] ]
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [fn]
          n = case Map.elems (extractionNodes ex) of (x:_) -> x; [] -> error "empty extractionNodes"
      nodeLabel n `shouldBe` fromText "retrieveClient"

    it "ids are stable run to run (determinism)" $ do
      let mk = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget
             [tsNode "program" "module" [func "a", cls "B", constant "C"]]
      map nodeId (Map.elems (extractionNodes (mk `seq` mk))) `shouldBe` map nodeId (Map.elems (extractionNodes mk))

    it "largest id and signature stay within 512 bytes on a large fixture" $ do
      let big = tsNode "program" "module"
             [ func (T.concat ["v", T.replicate 400 "erylongname"]) ]
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [big]
          nodes = Map.elems (extractionNodes ex)
      all (\n -> T.length (nodeId n) <= 512 &&
                 maybe True ((<= 512) . T.length . toText) (nodeSignature n)) nodes `shouldBe` True

  describe "signatures" $ do
    it "function signature is the normalized declaration text" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [func "f"]
          sig = nodeSignature (case Map.elems (extractionNodes ex) of (x:_) -> x; [] -> error "empty extractionNodes")
      sig `shouldBe` Just (fromText "function f")

    it "import signature keeps the trailing specifier clause" $ do
      let ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget
             [importDecl "'../../templates/index.js'"]
          is = filter (\m -> nodeKind m == Just (fromText "Import")) (Map.elems (extractionNodes ex))
          sig = nodeSignature (case is of (x:_) -> x; [] -> case Map.elems (extractionNodes ex) of (y:_) -> y; [] -> error "empty extractionNodes")
      sig `shouldSatisfy` maybe False (\x -> T.isSuffixOf "from '../../templates/index.js'" (toText x))

  describe "issue #77 — no bogus import from prose in a template literal" $ do
    it "emits no Import node when the only 'from ' is inside a template literal" $ do
      let prose = "## Examples by kind\npick one from the list below"
          node = tsNode "export_statement" (T.pack ("export const help = `" ++ T.unpack prose ++ "`;"))
                   [ tsNode "lexical_declaration" (T.pack ("const help = `" ++ T.unpack prose ++ "`;"))
                       [ tsNode "template_string" prose [] ] ]
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [node]
          imports = filter (\m -> nodeKind m == Just (fromText "Import")) (Map.elems (extractionNodes ex))
      imports `shouldBe` []

  describe "AVI-1447 calls edges" $ do
    let callsOf ex = [ (edgeSource e, edgeTarget e) | e <- Map.elems (extractionEdges ex), edgeRelation e == Calls ]
        callNode ty callee = tsNode ty (T.pack (T.unpack callee ++ "()")) [ident callee]
        fnWith name body = tsNode "function_declaration" (T.pack ("function " ++ T.unpack name))
                             [ident name, tsNode "statement_block" "{}" body]
        run ns = tsNodesToExtraction sf GranularityFine defaultTruncationBudget [tsNode "program" "module" ns]

    it "same-file call emits an edge from the enclosing function to the callee" $ do
      let ex = run [ func "a", fnWith "b" [callNode "call_expression" "a"] ]
      callsOf ex `shouldBe` [ (makeNodeId sf "Function" "b", makeNodeId sf "Function" "a") ]

    it "unresolvable callee (a global) emits no edge" $ do
      let ex = run [ fnWith "b" [callNode "new_expression" "Date"] ]
      callsOf ex `shouldBe` []

    it "only the top-level call of a nested call emits an edge" $ do
      let inner = callNode "call_expression" "a"
          outer = tsNode "call_expression" "c(a())" [ident "c", tsNode "arguments" "(a())" [inner]]
          ex = run [ func "a", func "c", fnWith "b" [outer] ]
      callsOf ex `shouldBe` [ (makeNodeId sf "Function" "b", makeNodeId sf "Function" "c") ]

    it "an imported callee targets the import's target node" $ do
      let imp = tsNode "import_statement" "import { Foo } from './foo'" [tsNode "string" "'./foo'" []]
          ex = run [ imp, fnWith "b" [callNode "new_expression" "Foo"] ]
      length (callsOf ex) `shouldBe` 1

  describe "3.6 per-language calls edges (extraction-semantic-enrichment)" $ do
    let callsOf ex = [ (edgeSource e, edgeTarget e) | e <- Map.elems (extractionEdges ex), edgeRelation e == Calls ]
        run ns = tsNodesToExtraction sf GranularityFine defaultTruncationBudget [tsNode "program" "module" ns]
        fnWith name body = tsNode "function_declaration" (T.pack ("function " ++ T.unpack name))
                             [ident name, tsNode "statement_block" "{}" body]
        -- A Java-shaped method body wrapping a single statement, so the call
        -- inside is reachable at fine granularity.
        javaMethodBody name stmt =
          tsNode "method_declaration" (T.pack (name ++ "() {}"))
            [ withField "name" (ident (T.pack name))
            , tsNode "block" "{}" [stmt] ]
        methodInvocation receiverM name =
          tsNode "method_invocation"
            (T.pack (maybe "" (++ ".") receiverM ++ name ++ "()"))
            (maybe [] (\r -> [withField "object" (ident (T.pack r))]) receiverM
              ++ [withField "name" (ident (T.pack name))])
        objectCreation ty = tsNode "object_creation_expression" (T.pack ("new " ++ ty ++ "()"))
          [withField "type" (tsNode "type_identifier" (T.pack ty) [])]

    it "Java: intra-class call resolves to the sibling method, not just top-level functions" $ do
      let cls' = clsWith "ClientApi"
            [ javaMethodBody "retrieveClient" (methodInvocation Nothing "helper")
            , javaMethodBody "helper" (tsNode "block" "{}" []) ]
          ex = run [ cls' ]
      callsOf ex `shouldBe`
        [ (makeNodeId sf "Method" "retrieveClient", makeNodeId sf "Method" "helper") ]

    it "Java: a receiver call resolves via the method name, not the receiver" $ do
      let cls' = clsWith "ClientApi"
            [ javaMethodBody "retrieveClient" (methodInvocation (Just "clientRepository") "save")
            , javaMethodBody "save" (tsNode "block" "{}" []) ]
          ex = run [ cls' ]
      callsOf ex `shouldBe`
        [ (makeNodeId sf "Method" "retrieveClient", makeNodeId sf "Method" "save") ]

    it "Java: a receiver call targets nothing when the method name itself is unresolvable" $ do
      let cls' = clsWith "ClientApi"
            [ javaMethodBody "retrieveClient" (methodInvocation (Just "clientRepository") "save") ]
          ex = run [ cls' ]
          edges = callsOf ex
      edges `shouldBe` []
      any (\(_, t) -> T.isInfixOf "clientRepository" t) edges `shouldBe` False

    it "Java: object creation via the 'type' field targets the imported class" $ do
      -- `collectCallsCtx`'s import-binding heuristic parses the TS/JS
      -- `import { X } from '...'` shape; a real Java `import a.b.ClientData;`
      -- resolves through the separate, grammar-aware Imports.hs pipeline in
      -- production. This fixture's import text only needs to exercise that
      -- heuristic — the behavior under test is the `type` field resolution.
      let imp = tsNode "import_statement" "import { ClientData } from './data'" [tsNode "string" "'./data'" []]
          cls' = clsWith "ClientApi" [javaMethodBody "helper" (objectCreation "ClientData")]
          ex = run [ imp, cls' ]
      length (callsOf ex) `shouldBe` 1

    it "PHP: member_call_expression resolves via the 'name' field, not the 'object' receiver" $ do
      let phpMethod name stmt =
            tsNode "method_declaration" (T.pack (name ++ "() {}"))
              [ withField "name" (ident (T.pack name)), stmt ]
          memberCall receiverM name =
            tsNode "member_call_expression" (T.pack (receiverM ++ "->" ++ name ++ "()"))
              [ withField "object" (ident (T.pack receiverM))
              , withField "name" (ident (T.pack name)) ]
          cls' = clsWith "ClientApi"
            [ phpMethod "retrieveClient" (memberCall "repo" "save")
            , phpMethod "save" (tsNode "block" "{}" []) ]
          ex = run [ cls' ]
      callsOf ex `shouldBe`
        [ (makeNodeId sf "Method" "retrieveClient", makeNodeId sf "Method" "save") ]

    it "PHP: object_creation_expression (no fields in this grammar) still resolves via positional fallback" $ do
      -- See the Java "object creation" test above re: why this import uses
      -- the TS/JS shape `collectCallsCtx`'s heuristic parses, rather than
      -- real PHP `use` syntax.
      let imp = tsNode "import_statement" "import { ClientData } from './data'" [tsNode "string" "'./data'" []]
          -- tree-sitter-php's object_creation_expression carries no field
          -- metadata at all (verified via node-types.json) — the class name
          -- is just a positional child, exercising the firstIdentifierInList
          -- fallback in calleeNameOf.
          newExpr = tsNode "object_creation_expression" "new ClientData()" [ident "ClientData"]
          phpMethod name stmt =
            tsNode "method_declaration" (T.pack (name ++ "() {}")) [withField "name" (ident (T.pack name)), stmt]
          cls' = clsWith "ClientApi" [phpMethod "helper" newExpr]
          ex = run [ imp, cls' ]
      length (callsOf ex) `shouldBe` 1

    it "Ruby: method_call wrapping a nested dotted call resolves to the final segment" $ do
      let rubyMethod name stmt =
            tsNode "method" (T.pack (name ++ "; end")) [withField "name" (ident (T.pack name)), stmt]
          dottedCall receiverM name =
            -- `obj.foo` with no parens/args parses as a bare `call` node
            -- (field `method` + `receiver`); `method_call` wraps it when
            -- there ARE parens/args (field `method` points at the `call`).
            tsNode "method_call" (T.pack (receiverM ++ "." ++ name ++ "(x)"))
              [ withField "method"
                  (tsNode "call" (T.pack (receiverM ++ "." ++ name))
                    [ withField "receiver" (ident (T.pack receiverM))
                    , withField "method" (ident (T.pack name)) ]) ]
          cls' = clsWith "ClientApi"
            [ rubyMethod "retrieve_client" (dottedCall "repo" "save")
            , rubyMethod "save" (tsNode "block" "{}" []) ]
          ex = run [ cls' ]
      callsOf ex `shouldBe`
        [ (makeNodeId sf "Method" "retrieve_client", makeNodeId sf "Method" "save") ]

    it "TypeScript: a member call resolves to the method, not the receiver (supersedes the original AVI-1447 shape)" $ do
      let memberExpr receiverM name =
            tsNode "member_expression" (T.pack (receiverM ++ "." ++ name))
              [ withField "object" (ident (T.pack receiverM))
              , withField "property" (tsNode "property_identifier" (T.pack name) []) ]
          tsCall receiverM name =
            tsNode "call_expression" (T.pack (receiverM ++ "." ++ name ++ "()"))
              [withField "function" (memberExpr receiverM name)]
          ex = run [ func "save", fnWith "b" [tsCall "svc" "save"] ]
      callsOf ex `shouldBe` [ (makeNodeId sf "Function" "b", makeNodeId sf "Function" "save") ]

  describe "3.7 structured annotations (extraction-semantic-enrichment)" $ do
    let run ns = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [tsNode "program" "module" ns]
        javaMarker name = tsNode "marker_annotation" (T.pack ("@" ++ T.unpack name))
          [withField "name" (ident name)]
        javaAnn name args = tsNode "annotation" (T.pack ("@" ++ T.unpack name ++ "(" ++ T.unpack args ++ ")"))
          [ withField "name" (ident name)
          , withField "arguments" (tsNode "annotation_argument_list" (T.pack ("(" ++ T.unpack args ++ ")")) [] ) ]
        -- Java method carrying a `modifiers` child (the real annotation
        -- carrier, verified via node-types.json) and a field-tagged `body`
        -- so the header/body split under test actually has a body to drop.
        javaMethodAnn anns name bodyText =
          tsNode "method_declaration"
            (T.concat (map tsnText anns) <> " public ClientData " <> name <> "(final long id) " <> bodyText)
            [ tsNode "modifiers" (T.concat (map tsnText anns)) anns
            , tsNode "type_identifier" "ClientData" []
            , withField "name" (ident name)
            , withField "body" (tsNode "block" bodyText [])
            ]
        javaClassAnn anns name children =
          tsNode "class_declaration" (T.concat (map tsnText anns) <> " class " <> name)
            (tsNode "modifiers" (T.concat (map tsnText anns)) anns : ident name : children)
        javaFieldAnn anns ty name =
          tsNode "field_declaration" (T.concat (map tsnText anns) <> " " <> ty <> " " <> name <> ";")
            [ tsNode "modifiers" (T.concat (map tsnText anns)) anns
            , tsNode "type_identifier" ty []
            , withField "declarator" (tsNode "variable_declarator" name [withField "name" (ident name)])
            ]
        extraOf n = nodeExtra n

    it "annotations are captured as a structured array on the annotated node" $ do
      let m = javaMethodAnn [javaMarker "GET", javaAnn "Path" "\"/{id}\""] "retrieveClient" "{ return helper(id); }"
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [m]
          n = case Map.elems (extractionNodes ex) of (x:_) -> x; [] -> error "empty"
      extraOf n `shouldBe` Just (object
        [ "annotations" .= [ object ["name" .= ("GET" :: T.Text)]
                           , object ["name" .= ("Path" :: T.Text), "args" .= ("(\"/{id}\")" :: T.Text)] ]
        , "httpVerb" .= ("GET" :: T.Text)
        , "route" .= ("/{id}" :: T.Text)
        ])

    it "the method's signature keeps the annotations and drops the body" $ do
      let m = javaMethodAnn [javaMarker "GET", javaAnn "Path" "\"/{id}\""] "retrieveClient"
                "{ ClientData d = helper(id); return d; }"
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [m]
          n = case Map.elems (extractionNodes ex) of (x:_) -> x; [] -> error "empty"
      case nodeSignature n of
        Just sig -> do
          T.isInfixOf "@GET" (toText sig) `shouldBe` True
          T.isInfixOf "retrieveClient" (toText sig) `shouldBe` True
          T.isInfixOf "helper(id)" (toText sig) `shouldBe` False
        Nothing -> fail "expected a signature"

    it "a class-level @Path distills to route" $ do
      let c = javaClassAnn [javaAnn "Path" "\"/v1/clients\""] "ClientApi" []
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [c]
          n = case Map.elems (extractionNodes ex) of (x:_) -> x; [] -> error "empty"
      extraOf n `shouldBe` Just (object
        [ "annotations" .= [ object ["name" .= ("Path" :: T.Text), "args" .= ("(\"/v1/clients\")" :: T.Text)] ]
        , "route" .= ("/v1/clients" :: T.Text)
        ])

    it "@Scheduled(cron = ...) distills to cron" $ do
      let m = javaMethodAnn [javaAnn "Scheduled" "cron = \"0 0 * * * *\""] "refreshCache" "{}"
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [m]
          n = case Map.elems (extractionNodes ex) of (x:_) -> x; [] -> error "empty"
      nodeExtra n `shouldSatisfy` maybe False (T.isInfixOf "0 0 * * * *" . T.pack . show)

    it "@Value(\"${...}\") distills to configKeys" $ do
      let f = javaFieldAnn [javaAnn "Value" "\"${fineract.tenant.host}\""] "String" "tenantHost"
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [f]
          n = case Map.elems (extractionNodes ex) of (x:_) -> x; [] -> error "empty"
      nodeLabel n `shouldBe` fromText "tenantHost"
      nodeExtra n `shouldSatisfy` maybe False (T.isInfixOf "fineract.tenant.host" . T.pack . show)

    it "a resolvable annotation emits a references edge to the imported symbol" $ do
      -- See the stream-2 Java "object creation" tests re: this import shape
      -- (collectCallsCtx's heuristic parses TS/JS `import { X } from '...'`;
      -- real Java `import javax.ws.rs.GET;` resolves through the separate
      -- Imports.hs pipeline, not exercised by this unit test).
      let imp = tsNode "import_statement" "import { GET } from './jaxrs'" [tsNode "string" "'./jaxrs'" []]
          m = javaMethodAnn [javaMarker "GET"] "retrieveClient" "{}"
          ex = run [ imp, m ]
          refs = [ (edgeSource e, edgeTarget e) | e <- Map.elems (extractionEdges ex), edgeRelation e == References ]
      length refs `shouldBe` 1

    it "Python: a decorator on decorated_definition is captured on the inner function" $ do
      let dec = tsNode "decorator" "@app.route(\"/clients\")" []
          inner = withField "definition" (func "retrieve_client")
          wrapped = tsNode "decorated_definition" "@app.route(\"/clients\")\ndef retrieve_client(): pass" [dec, inner]
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [wrapped]
          n = case Map.elems (extractionNodes ex) of (x:_) -> x; [] -> error "empty"
      nodeLabel n `shouldBe` fromText "retrieve_client"
      nodeExtra n `shouldBe` Just (object
        [ "exported" .= True
        , "annotations" .= [ object ["name" .= ("route" :: T.Text), "args" .= ("(\"/clients\")" :: T.Text)] ]
        ])

    it "TypeScript: a class's repeatable decorator field is captured" $ do
      let dec = tsNode "decorator" "@Component" []
          c = tsNode "class_declaration" "@Component\nclass Widget {}" [withField "decorator" dec, ident "Widget"]
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [c]
          n = case Map.elems (extractionNodes ex) of (x:_) -> x; [] -> error "empty"
      nodeExtra n `shouldBe` Just (object [ "annotations" .= [ object ["name" .= ("Component" :: T.Text)] ] ])

  describe "3.8 notable strings: env/config/topic (extraction-semantic-enrichment)" $ do
    let notableNodesOf ex = [ n | n <- Map.elems (extractionNodes ex)
                                 , nodeKind n `elem` map (Just . fromText) ["Env", "Config", "Topic"] ]
        refsOf ex = [ (edgeSource e, edgeTarget e) | e <- Map.elems (extractionEdges ex), edgeRelation e == References ]
        javaCall name argText =
          tsNode "method_invocation" (name <> "(" <> argText <> ")")
            [ withField "name" (ident (T.takeWhileEnd (/= '.') name))
            , withField "arguments" (tsNode "argument_list" ("(" <> argText <> ")")
                [tsNode "string_literal" argText []]) ]
        methodWithBody name stmt =
          tsNode "method_declaration" (name <> "() {}")
            [withField "name" (ident name), withField "body" (tsNode "block" "" [stmt])]

    it "Java System.getenv(\"X\") at Function granularity (opaque body) still yields env:X" $ do
      let m = methodWithBody "helper" (javaCall "System.getenv" "\"CLIENTS_TABLE\"")
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [m]
      map nodeLabel (notableNodesOf ex) `shouldBe` [fromText "CLIENTS_TABLE"]
      refsOf ex `shouldBe` [ (makeNodeId sf "Method" "helper", "env:CLIENTS_TABLE") ]

    it "process.env.X (member access, not a call) yields env:X" $ do
      let memberAccess = tsNode "member_expression" "process.env.CLIENTS_TABLE"
            [ withField "object" (tsNode "member_expression" "process.env"
                [withField "object" (ident "process"), withField "property" (tsNode "property_identifier" "env" [])])
            , withField "property" (tsNode "property_identifier" "CLIENTS_TABLE" []) ]
          m = methodWithBody "helper" memberAccess
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [m]
      map nodeLabel (notableNodesOf ex) `shouldBe` [fromText "CLIENTS_TABLE"]

    it "Ruby ENV[\"X\"] (subscript form) yields env:X" $ do
      let subscript = tsNode "element_reference" "ENV[\"CLIENTS_TABLE\"]" [ident "ENV", tsNode "string" "\"CLIENTS_TABLE\"" []]
          m = methodWithBody "helper" subscript
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [m]
      map nodeLabel (notableNodesOf ex) `shouldBe` [fromText "CLIENTS_TABLE"]

    it "the same env var read from two call sites converges on one node with two edges" $ do
      let m1 = methodWithBody "helperA" (javaCall "System.getenv" "\"CLIENTS_TABLE\"")
          m2 = methodWithBody "helperB" (javaCall "System.getenv" "\"CLIENTS_TABLE\"")
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [m1, m2]
      length (notableNodesOf ex) `shouldBe` 1
      length (refsOf ex) `shouldBe` 2

    it "@KafkaListener distills to a canonical topic: node and edge" $ do
      let ann = tsNode "annotation" "@KafkaListener(\"orders\")"
            [ withField "name" (ident "KafkaListener")
            , withField "arguments" (tsNode "annotation_argument_list" "(\"orders\")" []) ]
          m = tsNode "method_declaration" "@KafkaListener(\"orders\") void onOrder() {}"
                [tsNode "modifiers" "" [ann], withField "name" (ident "onOrder")]
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [m]
      map nodeLabel (notableNodesOf ex) `shouldBe` [fromText "orders"]
      refsOf ex `shouldBe` [ (makeNodeId sf "Method" "onOrder", "topic:orders") ]

    it "@Value(\"${...}\") distills to a canonical config: node and edge" $ do
      let ann = tsNode "annotation" "@Value(\"${fineract.tenant.host}\")"
            [ withField "name" (ident "Value")
            , withField "arguments" (tsNode "annotation_argument_list" "(\"${fineract.tenant.host}\")" []) ]
          f = tsNode "field_declaration" "@Value(\"${fineract.tenant.host}\") String tenantHost;"
                [ tsNode "modifiers" "" [ann]
                , tsNode "type_identifier" "String" []
                , withField "declarator" (tsNode "variable_declarator" "tenantHost" [withField "name" (ident "tenantHost")]) ]
          ex = tsNodesToExtraction sf GranularityFunction defaultTruncationBudget [f]
      map nodeLabel (notableNodesOf ex) `shouldBe` [fromText "fineract.tenant.host"]
      refsOf ex `shouldBe` [ (makeNodeId sf "Field" "tenantHost", "config:fineract.tenant.host") ]

    it "over-cap: more than 32 distinct names in one file keeps only 32" $ do
      let names = [ T.pack ("VAR" ++ show i) | i <- [1 .. 40 :: Int] ]
          calls = [ javaCall "System.getenv" (T.pack ("\"" ++ T.unpack nm ++ "\"")) | nm <- names ]
          m = methodWithBody "helper" (tsNode "block" "" calls)
          ex = tsNodesToExtraction sf GranularityFine defaultTruncationBudget [m]
      length (notableNodesOf ex) `shouldBe` 32

-- | Attach a start row to a fixture node for collision ordering.
withLine :: TSNodeInfo -> Int -> TSNodeInfo
withLine n l = n { tsnStartRow = l }

sortIds :: [T.Text] -> [T.Text]
sortIds = Map.keys . Map.fromList . map (\x -> (x, x))
