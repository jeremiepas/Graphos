module Studio.Data.Graph exposing
    ( Aggregate
    , Edge
    , FileType(..)
    , Graph
    , Node
    , adjacency
    , aggregateFor
    , communityLabel
    , communityMembers
    , decoder
    , edgeCount
    , edgeDecoder
    , empty
    , encodeContract
    , encodeEdge
    , encodeNode
    , fileTypeFromString
    , fileTypeToString
    , fingerprint
    , incidentEdges
    , neighborhood
    , nodeCount
    , nodeDecoder
    , synthesizeAggregates
    )

{-| The graph-json-contract shape (mirrors `Graphos.Domain.Types.Node` /
`Graphos.Domain.Types.Edge`): 12-field nodes, 7-field edges, plus the
`community_aggregates` dataset from `html-lod-viewer` (synthesized client-side
when the export predates it). One decoder for file mode and connected mode.
-}

import Bitwise
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)



-- TYPES


type FileType
    = CodeFile
    | DocFile
    | PaperFile
    | ImageFile
    | VideoFile
    | AudioFile
    | OfficeFile


type alias Node =
    { id : String
    , label : String
    , fileType : FileType
    , sourceFile : String
    , lineStart : Maybe Int
    , lineEnd : Maybe Int
    , signature : Maybe String
    , communityId : Maybe Int
    , kind : Maybe String
    , degree : Maybe Int
    , isBridge : Maybe Bool
    , extra : Maybe D.Value
    }


type alias Edge =
    { id : String
    , source : String
    , target : String
    , relation : String
    , weight : Maybe Float
    , confidence : Maybe Float
    , extra : Maybe D.Value
    }


type alias Aggregate =
    { id : Int
    , memberCount : Int
    , cohesion : Float
    , bridgeCount : Int
    , color : String
    , label : String
    , representativeLabels : List String
    }


type alias Graph =
    { nodes : Dict String Node
    , edges : Dict String Edge
    , aggregates : List Aggregate
    , communityLabels : Dict Int String
    }


empty : Graph
empty =
    { nodes = Dict.empty, edges = Dict.empty, aggregates = [], communityLabels = Dict.empty }


nodeCount : Graph -> Int
nodeCount g =
    Dict.size g.nodes


edgeCount : Graph -> Int
edgeCount g =
    Dict.size g.edges



-- FILE TYPE


fileTypeToString : FileType -> String
fileTypeToString ft =
    case ft of
        CodeFile ->
            "code"

        DocFile ->
            "doc"

        PaperFile ->
            "paper"

        ImageFile ->
            "image"

        VideoFile ->
            "video"

        AudioFile ->
            "audio"

        OfficeFile ->
            "office"


fileTypeFromString : String -> Maybe FileType
fileTypeFromString s =
    case s of
        "code" ->
            Just CodeFile

        "doc" ->
            Just DocFile

        "paper" ->
            Just PaperFile

        "image" ->
            Just ImageFile

        "video" ->
            Just VideoFile

        "audio" ->
            Just AudioFile

        "office" ->
            Just OfficeFile

        _ ->
            Nothing



-- DECODERS


optField : String -> D.Decoder a -> D.Decoder (Maybe a)
optField name d =
    D.oneOf [ D.field name (D.nullable d), D.succeed Nothing ]


nodeDecoder : D.Decoder Node
nodeDecoder =
    let
        fileType =
            D.string
                |> D.andThen
                    (\s ->
                        case fileTypeFromString s of
                            Just ft ->
                                D.succeed ft

                            Nothing ->
                                D.fail ("Unknown file type: " ++ s)
                    )
    in
    D.map8
        (\id label ft sf ls le sig cid ->
            { id = id
            , label = label
            , fileType = ft
            , sourceFile = sf
            , lineStart = ls
            , lineEnd = le
            , signature = sig
            , communityId = cid
            , kind = Nothing
            , degree = Nothing
            , isBridge = Nothing
            , extra = Nothing
            }
        )
        (D.field "id" D.string)
        (D.field "label" D.string)
        (D.field "file_type" fileType)
        (D.field "source_file" D.string)
        (optField "line_start" D.int)
        (optField "line_end" D.int)
        (optField "signature" D.string)
        (optField "community_id" D.int)
        |> D.andThen
            (\n ->
                D.map4
                    (\kind degree isBridge extra ->
                        { n | kind = kind, degree = degree, isBridge = isBridge, extra = extra }
                    )
                    (optField "kind" D.string)
                    (optField "degree" D.int)
                    (optField "is_bridge" D.bool)
                    (optField "extra" D.value)
            )


edgeDecoder : D.Decoder Edge
edgeDecoder =
    D.map7 Edge
        (D.field "id" D.string)
        (D.field "source" D.string)
        (D.field "target" D.string)
        (D.field "relation" D.string)
        (optField "weight" D.float)
        (optField "confidence" D.float)
        (optField "extra" D.value)


aggregateDecoder : D.Decoder Aggregate
aggregateDecoder =
    D.map7 Aggregate
        (D.field "id" D.int)
        (D.field "member_count" D.int)
        (D.oneOf [ D.field "cohesion" D.float, D.succeed 0 ])
        (D.oneOf [ D.field "bridge_count" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "color" D.string, D.succeed "hsl(0,0%,53%)" ])
        (D.oneOf [ D.field "label" D.string, D.succeed "" ])
        (D.oneOf [ D.field "representative_labels" (D.list D.string), D.succeed [] ])


communityLabelsDecoder : D.Decoder (Dict Int String)
communityLabelsDecoder =
    D.keyValuePairs D.string
        |> D.map
            (List.filterMap
                (\( k, v ) -> Maybe.map (\i -> ( i, v )) (String.toInt k))
                >> Dict.fromList
            )


{-| Decode a full graph.json. `community_aggregates` and `community_labels`
are optional; aggregates are synthesized when absent.
-}
decoder : D.Decoder Graph
decoder =
    D.map4
        (\nodes edges aggregates labels ->
            let
                g =
                    { nodes = Dict.fromList (List.map (\n -> ( n.id, n )) nodes)
                    , edges = Dict.fromList (List.map (\e -> ( e.id, e )) edges)
                    , aggregates = aggregates
                    , communityLabels = labels
                    }
            in
            if List.isEmpty aggregates then
                { g | aggregates = synthesizeAggregates g }

            else
                g
        )
        (D.field "nodes" (D.list nodeDecoder))
        (D.field "edges" (D.list edgeDecoder))
        (D.oneOf [ D.field "community_aggregates" (D.list aggregateDecoder), D.succeed [] ])
        (D.oneOf [ D.field "community_labels" communityLabelsDecoder, D.succeed Dict.empty ])



-- AGGREGATE SYNTHESIS (exports that predate community_aggregates)


{-| Build aggregates from node community assignments: member counts, bridge
counts, deterministic palette colors, labels from `community_labels` with the
`"Community <id>"` fallback specified by `html-lod-viewer`.
-}
synthesizeAggregates : Graph -> List Aggregate
synthesizeAggregates g =
    let
        step _ n acc =
            case n.communityId of
                Nothing ->
                    acc

                Just cid ->
                    Dict.update cid
                        (\entry ->
                            let
                                ( members, bridges ) =
                                    Maybe.withDefault ( 0, 0 ) entry
                            in
                            Just
                                ( members + 1
                                , bridges
                                    + (if n.isBridge == Just True then
                                        1

                                       else
                                        0
                                      )
                                )
                        )
                        acc

        counts =
            Dict.foldl step Dict.empty g.nodes
    in
    Dict.toList counts
        |> List.map
            (\( cid, ( members, bridges ) ) ->
                { id = cid
                , memberCount = members
                , cohesion = 0
                , bridgeCount = bridges
                , color = paletteColor cid
                , label = communityLabel g cid
                , representativeLabels = []
                }
            )


{-| Deterministic distinguishable color per community id (golden-angle hue). -}
paletteColor : Int -> String
paletteColor cid =
    let
        hue =
            modBy 360 (cid * 137)
    in
    "hsl(" ++ String.fromInt hue ++ ",62%,52%)"


communityLabel : Graph -> Int -> String
communityLabel g cid =
    case Dict.get cid g.communityLabels of
        Just l ->
            l

        Nothing ->
            g.aggregates
                |> List.filter (\a -> a.id == cid && a.label /= "")
                |> List.head
                |> Maybe.map .label
                |> Maybe.withDefault ("Community " ++ String.fromInt cid)


aggregateFor : Graph -> Int -> Maybe Aggregate
aggregateFor g cid =
    List.head (List.filter (\a -> a.id == cid) g.aggregates)



-- TRAVERSAL


communityMembers : Graph -> Int -> List Node
communityMembers g cid =
    Dict.values g.nodes |> List.filter (\n -> n.communityId == Just cid)


incidentEdges : Graph -> String -> List Edge
incidentEdges g nid =
    Dict.values g.edges |> List.filter (\e -> e.source == nid || e.target == nid)


{-| Undirected adjacency: nodeId -> neighbor nodeIds. -}
adjacency : Graph -> Dict String (List String)
adjacency g =
    let
        add a b acc =
            Dict.update a (\m -> Just (b :: Maybe.withDefault [] m)) acc
    in
    Dict.foldl (\_ e acc -> acc |> add e.source e.target |> add e.target e.source)
        Dict.empty
        g.edges


{-| BFS neighborhood of a node up to `depth` hops (undirected), including the
start node.
-}
neighborhood : Graph -> String -> Int -> Set String
neighborhood g start depth =
    let
        adj =
            adjacency g

        go frontier visited remaining =
            if remaining <= 0 || Set.isEmpty frontier then
                visited

            else
                let
                    next =
                        Set.foldl
                            (\nid acc ->
                                Dict.get nid adj
                                    |> Maybe.withDefault []
                                    |> List.filter (\m -> not (Set.member m visited))
                                    |> List.foldl Set.insert acc
                            )
                            Set.empty
                            frontier
                in
                go next (Set.union visited next) (remaining - 1)
    in
    if Dict.member start g.nodes then
        go (Set.singleton start) (Set.singleton start) depth

    else
        Set.empty



-- CONTRACT ENCODING (export edited graphs; exportSubgraphJSON shape)


encodeNode : Node -> E.Value
encodeNode n =
    let
        opt name enc v =
            ( name, Maybe.withDefault E.null (Maybe.map enc v) )
    in
    E.object
        [ ( "id", E.string n.id )
        , ( "label", E.string n.label )
        , ( "file_type", E.string (fileTypeToString n.fileType) )
        , ( "source_file", E.string n.sourceFile )
        , opt "line_start" E.int n.lineStart
        , opt "line_end" E.int n.lineEnd
        , opt "signature" E.string n.signature
        , opt "community_id" E.int n.communityId
        , opt "kind" E.string n.kind
        , opt "degree" E.int n.degree
        , opt "is_bridge" E.bool n.isBridge
        , opt "extra" identity n.extra
        ]


encodeEdge : Edge -> E.Value
encodeEdge e =
    let
        opt name enc v =
            ( name, Maybe.withDefault E.null (Maybe.map enc v) )
    in
    E.object
        [ ( "id", E.string e.id )
        , ( "source", E.string e.source )
        , ( "target", E.string e.target )
        , ( "relation", E.string e.relation )
        , opt "weight" E.float e.weight
        , opt "confidence" E.float e.confidence
        , opt "extra" identity e.extra
        ]


{-| Contract-shaped graph.json (the `exportSubgraphJSON` layout, directly
consumable via `--graph`).
-}
encodeContract : Graph -> E.Value
encodeContract g =
    E.object
        [ ( "nodes", E.list encodeNode (Dict.values g.nodes) )
        , ( "edges", E.list encodeEdge (Dict.values g.edges) )
        , ( "communities", E.object [] )
        , ( "cohesion", E.object [] )
        , ( "god_nodes", E.list identity [] )
        , ( "community_labels"
          , E.object
                (Dict.toList g.communityLabels
                    |> List.map (\( k, v ) -> ( String.fromInt k, E.string v ))
                )
          )
        ]



-- IDENTITY


{-| Content fingerprint for file-mode graph identity: two independent rolling
hashes (djb2 + sdbm) over the raw text, hex-joined. Not cryptographic — a
stable identity key for persistence, per `studio-data-sources`.
-}
fingerprint : String -> String
fingerprint s =
    let
        step char ( h1, h2 ) =
            let
                c =
                    Char.toCode char
            in
            ( Bitwise.and 0xFFFFFFFF (Bitwise.shiftLeftBy 5 h1 + h1 + c)
            , Bitwise.and 0xFFFFFFFF (c + Bitwise.shiftLeftBy 6 h2 + Bitwise.shiftLeftBy 16 h2 - h2)
            )

        ( a, b ) =
            String.foldl step ( 5381, 0 ) s
    in
    toHex a ++ "-" ++ toHex b


toHex : Int -> String
toHex n =
    let
        go v acc =
            if v == 0 then
                if acc == "" then
                    "0"

                else
                    acc

            else
                go (v // 16) (String.slice (modBy 16 v) (modBy 16 v + 1) "0123456789abcdef" ++ acc)
    in
    go (Bitwise.shiftRightZfBy 0 n) ""
