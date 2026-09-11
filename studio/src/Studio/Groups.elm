module Studio.Groups exposing
    ( EvalMode(..)
    , Group
    , decodeSet
    , encodeSet
    , matchLocal
    , memberships
    , newGroup
    , visibleUnderIsolation
    )

{-| Query+color groups with `query-groups` semantics (`studio-groups`):
first-match-wins precedence by panel order, hide/isolate, group-set JSON
interchangeable with the viewer's. Local evaluation matches label, source
path and kind (documented subset); connected mode uses the server's
group-evaluation endpoint when probed.
-}

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Studio.Data.Graph exposing (Graph, Node)



-- TYPES


type alias Group =
    { name : String
    , query : String
    , color : String
    , hidden : Bool
    , isolated : Bool
    }


type EvalMode
    = ServerEval
    | LocalEval


newGroup : String -> String -> String -> Group
newGroup name query color =
    { name = name, query = query, color = color, hidden = False, isolated = False }



-- LOCAL EVALUATION (file mode / servers without the endpoint)


{-| Case-insensitive substring match over label, source path and kind — the
documented local subset. Every space-separated term must match somewhere.
-}
matchLocal : String -> Node -> Bool
matchLocal query node =
    let
        haystack =
            String.toLower
                (node.label
                    ++ " "
                    ++ node.sourceFile
                    ++ " "
                    ++ Maybe.withDefault "" node.kind
                )

        terms =
            -- String.words "" yields [""] and contains "" is always True;
            -- drop empties so a blank query matches nothing.
            String.words (String.toLower query)
                |> List.filter (\t -> t /= "")
    in
    not (List.isEmpty terms)
        && List.all (\t -> String.contains t haystack) terms


{-| First-match-wins membership: nodeId -> index of the first matching group
in panel order.
-}
memberships : List Group -> Graph -> Dict String Int
memberships groups graph =
    let
        indexed =
            List.indexedMap Tuple.pair groups

        firstMatch node =
            indexed
                |> List.filter (\( _, grp ) -> matchLocal grp.query node)
                |> List.head
                |> Maybe.map Tuple.first
    in
    Dict.foldl
        (\nid node acc ->
            case firstMatch node of
                Just i ->
                    Dict.insert nid i acc

                Nothing ->
                    acc
        )
        Dict.empty
        graph.nodes


{-| Isolation semantics: when at least one group is isolated, only members of
isolated groups are visible. Membership here is any-match (isolation is about
belonging), while color stays first-match-wins.
-}
visibleUnderIsolation : List Group -> Graph -> Maybe (Dict String Bool)
visibleUnderIsolation groups graph =
    let
        isolated =
            List.filter .isolated groups
    in
    if List.isEmpty isolated then
        Nothing

    else
        Just
            (Dict.map
                (\_ node ->
                    List.any (\grp -> matchLocal grp.query node) isolated
                )
                graph.nodes
            )



-- SET JSON (interchangeable with the viewer's group sets)


encodeGroup : Group -> E.Value
encodeGroup g =
    E.object
        [ ( "name", E.string g.name )
        , ( "query", E.string g.query )
        , ( "color", E.string g.color )
        , ( "hidden", E.bool g.hidden )
        , ( "isolated", E.bool g.isolated )
        ]


encodeSet : List Group -> E.Value
encodeSet groups =
    E.object
        [ ( "version", E.int 1 )
        , ( "groups", E.list encodeGroup groups )
        ]


groupDecoder : D.Decoder Group
groupDecoder =
    D.map5 Group
        (D.field "name" D.string)
        (D.field "query" D.string)
        (D.field "color" D.string)
        (D.oneOf [ D.field "hidden" D.bool, D.succeed False ])
        (D.oneOf [ D.field "isolated" D.bool, D.succeed False ])


decodeSet : D.Value -> Result D.Error (List Group)
decodeSet =
    D.decodeValue (D.field "groups" (D.list groupDecoder))
