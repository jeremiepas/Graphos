module Studio.Navigation exposing
    ( Position(..)
    , Tuning
    , ViewState
    , breadcrumb
    , decodeHash
    , encodeHash
    , escape
    , initialState
    , isPositionChange
    , rank
    , sanitize
    )

{-| The navigation model specified by `viewer-navigation` and verified by its
Lean model (openspec/changes/viewer-navigation/lean/):

  - `escape` strictly descends the hierarchy (`escape_rank_lt`) and reaches
    Overview in ≤ 2 steps (`escape_escape`)
  - `breadcrumb (escape p) == dropLast (breadcrumb p)` for non-Overview p
    (`breadcrumb_escape`)
  - `decodeHash (encodeHash s) == Just s` (`decode_encode`)
  - position changes push history, tuning changes replace
    (`isPositionChange`, `steps_nonpos_back`)

The elm-test suite asserts these invariants; keep it and the Lean model in
sync when extending the position type.

-}

import Url



-- TYPES


{-| The three breadcrumb levels. -}
type Position
    = Overview
    | Community Int
    | Node Int String


type alias Tuning =
    { hops : Int
    , facets : List String
    }


type alias ViewState =
    { pos : Position
    , tuning : Tuning
    }


initialState : ViewState
initialState =
    { pos = Overview, tuning = { hops = 2, facets = [] } }



-- HIERARCHY


{-| Hierarchical Escape: close the panel (node → its community), then up a
level (community → overview), then stay.
-}
escape : Position -> Position
escape pos =
    case pos of
        Node cid _ ->
            Community cid

        Community _ ->
            Overview

        Overview ->
            Overview


rank : Position -> Int
rank pos =
    case pos of
        Overview ->
            0

        Community _ ->
            1

        Node _ _ ->
            2


{-| The trail: Overview ▸ community ▸ node. -}
breadcrumb : Position -> List Position
breadcrumb pos =
    case pos of
        Overview ->
            [ Overview ]

        Community cid ->
            [ Overview, Community cid ]

        Node cid nid ->
            [ Overview, Community cid, Node cid nid ]


{-| Push-vs-replace classification: same position = tuning-only change =
replace; different position = push.
-}
isPositionChange : ViewState -> ViewState -> Bool
isPositionChange old new =
    old.pos /= new.pos


{-| Stale-reference check: a position the loaded graph does not contain
degrades to Overview, keeping the tuning.
-}
sanitize : (Position -> Bool) -> ViewState -> ViewState
sanitize valid state =
    if valid state.pos then
        state

    else
        { state | pos = Overview }



-- URL CODEC
--
-- Fragment format (all components percent-encoded where free-form):
--   #/o?h=2&f=doc,code
--   #/c/483?h=2
--   #/n/483/mod_Auth?h=2&f=doc


encodeHash : ViewState -> String
encodeHash state =
    let
        path =
            case state.pos of
                Overview ->
                    "/o"

                Community cid ->
                    "/c/" ++ String.fromInt cid

                Node cid nid ->
                    "/n/" ++ String.fromInt cid ++ "/" ++ Url.percentEncode nid

        facetPart =
            if List.isEmpty state.tuning.facets then
                ""

            else
                "&f=" ++ String.join "," (List.map Url.percentEncode state.tuning.facets)
    in
    "#" ++ path ++ "?h=" ++ String.fromInt state.tuning.hops ++ facetPart


decodeHash : String -> Maybe ViewState
decodeHash raw =
    let
        stripped =
            if String.startsWith "#" raw then
                String.dropLeft 1 raw

            else
                raw

        ( pathPart, queryPart ) =
            case String.split "?" stripped of
                [ p ] ->
                    ( p, "" )

                p :: rest ->
                    ( p, String.join "?" rest )

                [] ->
                    ( "", "" )

        segments =
            String.split "/" pathPart |> List.filter (\s -> s /= "")

        tuning =
            decodeQuery queryPart
    in
    Maybe.map2 ViewState (decodePath segments) tuning


decodePath : List String -> Maybe Position
decodePath segments =
    case segments of
        [ "o" ] ->
            Just Overview

        [ "c", cid ] ->
            Maybe.map Community (String.toInt cid)

        [ "n", cid, nid ] ->
            Maybe.map2 Node (String.toInt cid) (Url.percentDecode nid)

        _ ->
            Nothing


decodeQuery : String -> Maybe Tuning
decodeQuery query =
    let
        pairs =
            String.split "&" query
                |> List.filterMap
                    (\kv ->
                        case String.split "=" kv of
                            [ k, v ] ->
                                Just ( k, v )

                            _ ->
                                Nothing
                    )

        lookup key =
            pairs
                |> List.filter (\( k, _ ) -> k == key)
                |> List.head
                |> Maybe.map Tuple.second

        hops =
            lookup "h" |> Maybe.andThen String.toInt

        facets =
            case lookup "f" of
                Nothing ->
                    Just []

                Just raw ->
                    String.split "," raw
                        |> List.map Url.percentDecode
                        |> List.foldr (Maybe.map2 (::)) (Just [])
    in
    Maybe.map2 Tuning hops facets
