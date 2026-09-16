module SourceTest exposing (suite)

{-| `Studio.Data.Source`: capability→implementation resolution, file-mode size
guard, and the slice-API wire decoders (overview / community slice /
neighborhood). The slice endpoints are not shipped server-side yet, so the
decoder tests drive the client against the documented wire format directly. -}

import Expect
import Json.Decode as D
import String
import Studio.Api as Api
import Studio.Data.Source as Source
import Test exposing (Test, describe, test)


caps : Bool -> Api.Capabilities
caps slices =
    { query = True, slices = slices }


overviewJson : String
overviewJson =
    """
{
  "node_count": 1200000,
  "edge_count": 4000000,
  "community_count": 8000,
  "graph_hash": "sha:abc123",
  "community_aggregates": [
    { "id": 483, "member_count": 3, "cohesion": 0.82, "bridge_count": 1,
      "color": "hsl(210,62%,52%)", "label": "Authentication",
      "representative_labels": ["verifyToken"] }
  ]
}
"""


overviewHashAliasJson : String
overviewHashAliasJson =
    """
{
  "node_count": 12,
  "edge_count": 11,
  "community_count": 4,
  "hash": "fp:def456",
  "community_aggregates": []
}
"""


communitySliceJson : String
communitySliceJson =
    """
{
  "community_id": 483,
  "nodes": [ { "id": "n1", "label": "a", "file_type": "code", "source_file": "x" } ],
  "edges": [],
  "total_members": 12000,
  "total_internal_edges": 30000,
  "next_cursor": "page2"
}
"""


neighborhoodSliceJson : String
neighborhoodSliceJson =
    """
{
  "nodes": [ { "id": "n1", "label": "a", "file_type": "code", "source_file": "x" } ],
  "edges": [],
  "total_size": 40,
  "truncated": true
}
"""


suite : Test
suite =
    describe "Studio.Data.Source"
        [ describe "capability → implementation resolution"
            [ test "slices capability selects SlicesMode" <|
                \_ -> Source.resolveMode (caps True) |> Expect.equal Source.SlicesMode
            , test "no slices capability falls back to LegacyMode" <|
                \_ -> Source.resolveMode (caps False) |> Expect.equal Source.LegacyMode
            , test "slices affordances available only in SlicesMode" <|
                \_ ->
                    ( Source.slicesAvailable Source.SlicesMode, Source.slicesAvailable Source.LegacyMode )
                        |> Expect.equal ( True, False )
            , test "unavailability reason names the missing slice API" <|
                \_ ->
                    Expect.equal True (String.contains "no slice API" (Source.slicesReason Source.LegacyMode))
            ]
        , describe "source kind by origin + caps"
            [ test "nothing connected is file mode" <|
                \_ ->
                    Source.resolveKind (caps True) Nothing |> Expect.equal Source.FileSource
            , test "origin + slices selects slices source" <|
                \_ ->
                    Source.resolveKind (caps True) (Just "http://localhost:8080")
                        |> Expect.equal Source.SlicesSource
            , test "origin + no slices selects legacy source" <|
                \_ ->
                    Source.resolveKind (caps False) (Just "http://localhost:8080")
                        |> Expect.equal Source.LegacySource
            ]
        , describe "file-mode size guard"
            [ test "refuses above the documented threshold" <|
                \_ ->
                    Source.exceedsSizeLimit Source.sizeThresholdBytes (Source.sizeThresholdBytes + 1)
                        |> Expect.equal True
            , test "accepts at the threshold" <|
                \_ ->
                    Source.exceedsSizeLimit Source.sizeThresholdBytes Source.sizeThresholdBytes
                        |> Expect.equal False
            , test "accepts well below the threshold" <|
                \_ ->
                    Source.exceedsSizeLimit Source.sizeThresholdBytes (40 * 1024 * 1024)
                        |> Expect.equal False
            ]
        , describe "overview decoder (/api/overview)"
            [ test "decodes aggregates, totals and graph_hash" <|
                \_ ->
                    D.decodeString Source.overviewDecoder overviewJson
                        |> Result.map
                            (\o ->
                                ( ( o.nodeCount, o.edgeCount, o.communityCount )
                                , o.graphHash
                                , List.length o.aggregates
                                )
                            )
                        |> Expect.equal ( Ok ( ( 1200000, 4000000, 8000 ), "sha:abc123", 1 ) )
            , test "accepts the older `hash` field name and defaults missing totals" <|
                \_ ->
                    D.decodeString Source.overviewDecoder overviewHashAliasJson
                        |> Result.map (\o -> ( o.graphHash, o.nodeCount, o.edgeCount ))
                        |> Expect.equal ( Ok ( "fp:def456", 12, 11 ) )
            ]
        , describe "community slice decoder (/api/slice/community)"
            [ test "decodes page, totals and next cursor" <|
                \_ ->
                    D.decodeString Source.communitySliceDecoder communitySliceJson
                        |> Result.map
                            (\s ->
                                ( s.communityId
                                , List.length s.nodes
                                , ( s.totalMembers, s.totalInternalEdges, s.nextCursor )
                                )
                            )
                        |> Expect.equal ( Ok ( 483, 1, ( 12000, 30000, Just "page2" ) ) )
            , test "next cursor absent on the last page" <|
                \_ ->
                    let
                        last =
                            String.replace ",\n  \"next_cursor\": \"page2\"" "" communitySliceJson
                    in
                    D.decodeString Source.communitySliceDecoder last
                        |> Result.map .nextCursor
                        |> Expect.equal ( Ok Nothing )
            ]
        , describe "neighborhood slice decoder (/api/slice/neighborhood)"
            [ test "decodes page, total size and truncation flag" <|
                \_ ->
                    D.decodeString Source.neighborhoodSliceDecoder neighborhoodSliceJson
                        |> Result.map
                            (\s -> ( List.length s.nodes, s.totalSize, s.truncated ))
                        |> Expect.equal ( Ok ( 1, 40, True ) )
            ]
        ]
