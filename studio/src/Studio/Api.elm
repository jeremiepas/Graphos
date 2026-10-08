module Studio.Api exposing
    ( Capabilities
    , fetchOverview
    , fetchGraphString
    , groupCount
    , isSliceProbeOk
    , mutate
    , Overview
    , overviewDecoder
    , noCapabilities
    , probeQuery
    , probeSlices
    )

{-| The `graphos serve` client (`studio-data-sources`): capability probing at
connect time, graph fetch for the legacy path, `/api/cypher/mutate` for
connected editing, `/api/query` for server-assisted group counts.

The slice endpoints (`progressive-graph-interface`) are probed so the studio
lights up when they land server-side; until then `slices = False` routes
through the legacy full-fetch path.

-}

import Http
import Json.Decode as D
import Json.Encode as E
import Url
import Studio.Data.Graph as Graph



-- CAPABILITIES


type alias Capabilities =
    { query : Bool
    , slices : Bool
    }


noCapabilities : Capabilities
noCapabilities =
    { query = False, slices = False }


{-| Does /api/query answer? (Existing servers: yes under `graphos serve`.) -}
probeQuery : String -> (Bool -> msg) -> Cmd msg
probeQuery origin toMsg =
    Http.get
        { url = origin ++ "/api/query?q=__studio_probe__&budget=1"
        , expect = Http.expectWhatever (\r -> toMsg (isOk r))
        }


{-| Does the slice API exist? (progressive-graph-interface, not yet shipped.) -}
probeSlices : String -> (Bool -> msg) -> Cmd msg
probeSlices origin toMsg =
    Http.get
        { url = origin ++ "/api/overview"
        , expect =
            Http.expectJson (\result -> toMsg (isSliceProbeOk result)) overviewDecoder
        }


{-| Did the /api/overview probe report the slice API available? Only a body
that decodes as an Overview counts; both HTTP failures and a malformed
(undecodable) body are `Err`, so they report NOT available. Pure + tested. -}
isSliceProbeOk : Result Http.Error Overview -> Bool
isSliceProbeOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


isOk : Result Http.Error () -> Bool
isOk r =
    case r of
        Ok _ ->
            True

        Err _ ->
            False



-- GRAPH FETCH (legacy connected path)


fetchGraphString : String -> (Result String String -> msg) -> Cmd msg
fetchGraphString origin toMsg =
    Http.get
        { url = origin ++ "/graph.json"
        , expect =
            Http.expectStringResponse toMsg
                (\resp ->
                    case resp of
                        Http.GoodStatus_ _ body ->
                            Ok body

                        Http.BadStatus_ meta _ ->
                            Err ("graph.json: HTTP " ++ String.fromInt meta.statusCode)

                        Http.NetworkError_ ->
                            Err "network error — is graphos serve running?"

                        Http.Timeout_ ->
                            Err "timeout fetching graph.json"

                        Http.BadUrl_ u ->
                            Err ("bad URL: " ++ u)
                )
        }



{-| Aggregates + graph totals + content hash. Zero nodes or edges (per the slice
API spec): this is the only data a fresh remote-mode load requires. Defined in
this wire-format client so the slice probe (`probeSlices`) validates a
`/api/overview` body against the same schema it serves. -}
type alias Overview =
    { aggregates : List Graph.Aggregate
    , nodeCount : Int
    , edgeCount : Int
    , communityCount : Int
    , graphHash : String
    }


{-| Decode `/api/overview`. Accepts `graph_hash` or the older `hash` name. -}
overviewDecoder : D.Decoder Overview
overviewDecoder =
    D.map5 Overview
        (D.field "community_aggregates" (D.list Graph.aggregateDecoder))
        (D.oneOf [ D.field "node_count" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "edge_count" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "community_count" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "graph_hash" D.string, D.field "hash" D.string ])



{-| Fetch the overview (`GET /api/overview`, progressive-graph-interface). The
only data a fresh slices-mode load requires: aggregates + totals + graph hash.
Returns the raw body; `Main` decodes it with `Api.overviewDecoder`. -}
fetchOverview : String -> (Result String String -> msg) -> Cmd msg
fetchOverview origin toMsg =
    Http.get
        { url = origin ++ "/api/overview"
        , expect =
            Http.expectStringResponse toMsg
                (\resp ->
                    case resp of
                        Http.GoodStatus_ _ body ->
                            Ok body

                        Http.BadStatus_ meta _ ->
                            Err ("overview: HTTP " ++ String.fromInt meta.statusCode)

                        Http.NetworkError_ ->
                            Err "network error — is graphos serve running?"

                        Http.Timeout_ ->
                            Err "timeout fetching overview"

                        Http.BadUrl_ u ->
                            Err ("bad URL: " ++ u)
                )
        }





mutate : String -> String -> (Result String () -> msg) -> Cmd msg
mutate origin cypherQuery toMsg =
    Http.post
        { url = origin ++ "/api/cypher/mutate"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "query", E.string cypherQuery )
                    , ( "persist", E.bool True )
                    ]
                )
        , expect =
            Http.expectStringResponse toMsg
                (\resp ->
                    case resp of
                        Http.GoodStatus_ _ body ->
                            case D.decodeString (D.field "error" D.string) body of
                                Ok serverErr ->
                                    Err serverErr

                                Err _ ->
                                    Ok ()

                        Http.BadStatus_ meta body ->
                            case D.decodeString (D.field "error" D.string) body of
                                Ok serverErr ->
                                    Err serverErr

                                Err _ ->
                                    Err ("mutation rejected: HTTP " ++ String.fromInt meta.statusCode)

                        Http.NetworkError_ ->
                            Err "network error during mutation"

                        Http.Timeout_ ->
                            Err "mutation timed out"

                        Http.BadUrl_ u ->
                            Err ("bad URL: " ++ u)
                )
        }



-- SERVER-ASSISTED GROUP COUNTS
--
-- Until the group-evaluation endpoint (progressive-graph-interface) ships,
-- /api/query gives an approximate server-side count for a group query.


groupCount : String -> String -> (Result String Int -> msg) -> Cmd msg
groupCount origin query toMsg =
    Http.get
        { url = origin ++ "/api/query?q=" ++ Url.percentEncode query ++ "&budget=2000"
        , expect =
            Http.expectJson
                (\r -> toMsg (Result.mapError (\_ -> "query failed") r))
                (D.oneOf
                    [ D.field "nodes" (D.list D.value) |> D.map List.length
                    , D.succeed 0
                    ]
                )
        }
