module Studio.Edit exposing
    ( EditIntent(..)
    , UndoStack
    , applyIntent
    , decodeLog
    , describe
    , emptyStack
    , encodeLog
    , inverse
    , pushDone
    , redo
    , toCypher
    , undo
    , validate
    )

{-| Editing core (`studio-graph-editing`): typed edit intents validated against
the 12-field node schema, applied to the in-memory graph, translated to the
`cypher-mutation` write subset for connected mode, with inverses recorded at
accept time so one undo/redo mechanism serves both modes.
-}

import Dict
import Json.Decode as D
import Json.Encode as E
import Studio.Data.Graph as Graph exposing (Edge, Graph, Node)



-- INTENTS


type EditIntent
    = Relabel String String String -- nodeId, oldLabel, newLabel
    | Retype String (Maybe String) (Maybe String) -- nodeId, oldKind, newKind
    | CreateNode Node
    | DeleteNode Node (List Edge) -- node + incident edges, for the inverse
    | CreateEdge Edge
    | DeleteEdge Edge
    | RestoreNode Node (List Edge) -- inverse of DeleteNode: node + its edges back


describe : EditIntent -> String
describe intent =
    case intent of
        Relabel nid _ new ->
            "relabel " ++ nid ++ " → " ++ new

        Retype nid _ new ->
            "retype " ++ nid ++ " → " ++ Maybe.withDefault "(none)" new

        CreateNode n ->
            "create node " ++ n.id

        DeleteNode n edges ->
            "delete node " ++ n.id ++ " (+" ++ String.fromInt (List.length edges) ++ " edges)"

        CreateEdge e ->
            "create edge " ++ e.source ++ " → " ++ e.target

        DeleteEdge e ->
            "delete edge " ++ e.source ++ " → " ++ e.target

        RestoreNode n edges ->
            "restore node " ++ n.id ++ " (+" ++ String.fromInt (List.length edges) ++ " edges)"



-- VALIDATION (client-side, before any effect)


validate : Graph -> EditIntent -> Result String EditIntent
validate g intent =
    case intent of
        Relabel nid _ new ->
            if String.trim new == "" then
                Err "Label must not be empty"

            else if not (Dict.member nid g.nodes) then
                Err ("Unknown node: " ++ nid)

            else
                Ok intent

        Retype nid _ _ ->
            if Dict.member nid g.nodes then
                Ok intent

            else
                Err ("Unknown node: " ++ nid)

        CreateNode n ->
            if String.trim n.id == "" then
                Err "Node id must not be empty"

            else if Dict.member n.id g.nodes then
                Err ("Node id already exists: " ++ n.id)

            else if String.trim n.label == "" then
                Err "Label must not be empty"

            else
                Ok intent

        DeleteNode n _ ->
            if Dict.member n.id g.nodes then
                Ok intent

            else
                Err ("Unknown node: " ++ n.id)

        CreateEdge e ->
            if not (Dict.member e.source g.nodes) then
                Err ("Unknown source node: " ++ e.source)

            else if not (Dict.member e.target g.nodes) then
                Err ("Unknown target node: " ++ e.target)

            else if String.trim e.relation == "" then
                Err "Relation must not be empty"

            else if Dict.member e.id g.edges then
                Err ("Edge id already exists: " ++ e.id)

            else
                Ok intent

        DeleteEdge e ->
            if Dict.member e.id g.edges then
                Ok intent

            else
                Err ("Unknown edge: " ++ e.id)

        RestoreNode n _ ->
            if Dict.member n.id g.nodes then
                Err ("Node id already exists: " ++ n.id)

            else
                Ok intent



-- INVERSES (recorded at accept time; undo issues these)


inverse : EditIntent -> EditIntent
inverse intent =
    case intent of
        Relabel nid old new ->
            Relabel nid new old

        Retype nid old new ->
            Retype nid new old

        CreateNode n ->
            DeleteNode n []

        DeleteNode n edges ->
            -- Deleting removed the node AND its incident edges; the inverse
            -- restores both (applyIntent RestoreNode; connected mode issues
            -- one create statement per element).
            RestoreNode n edges

        CreateEdge e ->
            DeleteEdge e

        DeleteEdge e ->
            CreateEdge e

        RestoreNode n edges ->
            DeleteNode n edges



-- APPLY (file mode: fold intents over the graph)


applyIntent : EditIntent -> Graph -> Graph
applyIntent intent g =
    case intent of
        Relabel nid _ new ->
            { g | nodes = Dict.update nid (Maybe.map (\n -> { n | label = new })) g.nodes }

        Retype nid _ new ->
            { g | nodes = Dict.update nid (Maybe.map (\n -> { n | kind = new })) g.nodes }

        CreateNode n ->
            { g | nodes = Dict.insert n.id n g.nodes }

        DeleteNode n _ ->
            { g
                | nodes = Dict.remove n.id g.nodes
                , edges = Dict.filter (\_ e -> e.source /= n.id && e.target /= n.id) g.edges
            }

        CreateEdge e ->
            { g | edges = Dict.insert e.id e g.edges }

        DeleteEdge e ->
            { g | edges = Dict.remove e.id g.edges }

        RestoreNode n edges ->
            { g
                | nodes = Dict.insert n.id n g.nodes
                , edges = List.foldl (\e acc -> Dict.insert e.id e acc) g.edges edges
            }



-- CYPHER TRANSLATION (connected mode; cypher-mutation write subset)


cypherEscape : String -> String
cypherEscape s =
    String.replace "'" "\\'" s


nodeProps : Node -> String
nodeProps n =
    let
        prop key value =
            Just (key ++ ": '" ++ cypherEscape value ++ "'")

        optProp key mv =
            Maybe.map (\v -> key ++ ": '" ++ cypherEscape v ++ "'") mv
    in
    [ prop "id" n.id
    , prop "label" n.label
    , prop "file_type" (Graph.fileTypeToString n.fileType)
    , prop "source_file" n.sourceFile
    , optProp "kind" n.kind
    ]
        |> List.filterMap identity
        |> String.join ", "


toCypher : EditIntent -> String
toCypher intent =
    case intent of
        Relabel nid _ new ->
            "MATCH (n {id: '" ++ cypherEscape nid ++ "'}) SET n.label = '" ++ cypherEscape new ++ "'"

        Retype nid _ new ->
            case new of
                Just k ->
                    "MATCH (n {id: '" ++ cypherEscape nid ++ "'}) SET n.kind = '" ++ cypherEscape k ++ "'"

                Nothing ->
                    "MATCH (n {id: '" ++ cypherEscape nid ++ "'}) REMOVE n.kind"

        CreateNode n ->
            "CREATE (n {" ++ nodeProps n ++ "})"

        DeleteNode n _ ->
            "MATCH (n {id: '" ++ cypherEscape n.id ++ "'}) DETACH DELETE n"

        CreateEdge e ->
            "MATCH (a {id: '"
                ++ cypherEscape e.source
                ++ "'}), (b {id: '"
                ++ cypherEscape e.target
                ++ "'}) CREATE (a)-[:"
                ++ e.relation
                ++ "]->(b)"

        DeleteEdge e ->
            "MATCH (a {id: '"
                ++ cypherEscape e.source
                ++ "'})-[r:"
                ++ e.relation
                ++ "]->(b {id: '"
                ++ cypherEscape e.target
                ++ "'}) DELETE r"

        RestoreNode n edges ->
            -- Node first; edges are re-created by separate statements built by
            -- the caller (one statement per port of the cypher subset).
            "CREATE (n {" ++ nodeProps n ++ "})"



-- UNDO / REDO (one zipper for both modes)


type alias UndoStack =
    { done : List EditIntent -- newest first
    , undone : List EditIntent
    }


emptyStack : UndoStack
emptyStack =
    { done = [], undone = [] }


pushDone : EditIntent -> UndoStack -> UndoStack
pushDone intent stack =
    { done = intent :: stack.done, undone = [] }


{-| Undo: returns the inverse intent to apply plus the new stack. -}
undo : UndoStack -> Maybe ( EditIntent, UndoStack )
undo stack =
    case stack.done of
        [] ->
            Nothing

        intent :: rest ->
            Just ( inverse intent, { done = rest, undone = intent :: stack.undone } )


{-| Redo: returns the original intent to re-apply plus the new stack. -}
redo : UndoStack -> Maybe ( EditIntent, UndoStack )
redo stack =
    case stack.undone of
        [] ->
            Nothing

        intent :: rest ->
            Just ( intent, { done = intent :: stack.done, undone = rest } )



-- LOG PERSISTENCE (file mode; identity-keyed by the caller)


encodeIntent : EditIntent -> E.Value
encodeIntent intent =
    case intent of
        Relabel nid old new ->
            E.object [ ( "op", E.string "relabel" ), ( "node", E.string nid ), ( "old", E.string old ), ( "new", E.string new ) ]

        Retype nid old new ->
            E.object
                [ ( "op", E.string "retype" )
                , ( "node", E.string nid )
                , ( "old", Maybe.withDefault E.null (Maybe.map E.string old) )
                , ( "new", Maybe.withDefault E.null (Maybe.map E.string new) )
                ]

        CreateNode n ->
            E.object [ ( "op", E.string "create_node" ), ( "node", encodeNodeForLog n ) ]

        DeleteNode n edges ->
            E.object
                [ ( "op", E.string "delete_node" )
                , ( "node", encodeNodeForLog n )
                , ( "edges", E.list encodeEdgeForLog edges )
                ]

        CreateEdge e ->
            E.object [ ( "op", E.string "create_edge" ), ( "edge", encodeEdgeForLog e ) ]

        DeleteEdge e ->
            E.object [ ( "op", E.string "delete_edge" ), ( "edge", encodeEdgeForLog e ) ]

        RestoreNode n edges ->
            E.object
                [ ( "op", E.string "restore_node" )
                , ( "node", encodeNodeForLog n )
                , ( "edges", E.list encodeEdgeForLog edges )
                ]


{-| Log entries carry the FULL contract shape so undo of a deletion restores
every field, not a lossy subset.
-}
encodeNodeForLog : Node -> E.Value
encodeNodeForLog =
    Graph.encodeNode


encodeEdgeForLog : Edge -> E.Value
encodeEdgeForLog =
    Graph.encodeEdge


encodeLog : List EditIntent -> E.Value
encodeLog =
    E.list encodeIntent


nodeForLogDecoder : D.Decoder Node
nodeForLogDecoder =
    Graph.nodeDecoder


edgeForLogDecoder : D.Decoder Edge
edgeForLogDecoder =
    Graph.edgeDecoder


intentDecoder : D.Decoder EditIntent
intentDecoder =
    D.field "op" D.string
        |> D.andThen
            (\op ->
                case op of
                    "relabel" ->
                        D.map3 Relabel (D.field "node" D.string) (D.field "old" D.string) (D.field "new" D.string)

                    "retype" ->
                        D.map3 Retype
                            (D.field "node" D.string)
                            (D.field "old" (D.nullable D.string))
                            (D.field "new" (D.nullable D.string))

                    "create_node" ->
                        D.map CreateNode (D.field "node" nodeForLogDecoder)

                    "delete_node" ->
                        D.map2 DeleteNode (D.field "node" nodeForLogDecoder) (D.field "edges" (D.list edgeForLogDecoder))

                    "create_edge" ->
                        D.map CreateEdge (D.field "edge" edgeForLogDecoder)

                    "delete_edge" ->
                        D.map DeleteEdge (D.field "edge" edgeForLogDecoder)

                    "restore_node" ->
                        D.map2 RestoreNode (D.field "node" nodeForLogDecoder) (D.field "edges" (D.list edgeForLogDecoder))

                    _ ->
                        D.fail ("Unknown edit op: " ++ op)
            )


decodeLog : D.Value -> Result D.Error (List EditIntent)
decodeLog =
    D.decodeValue (D.list intentDecoder)
