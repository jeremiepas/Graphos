module ApiTest exposing (suite)

{-| `Studio.Api` slice probe wiring (`studio-data-sources`): a probe reports the
slice API available only when the `/api/overview` body decodes as an Overview.
Both HTTP failures and a malformed body are `Err`, so they report NOT available. -}

import Expect
import Http
import Json.Decode as D
import Studio.Api as Api
import Test exposing (Test, describe, test)


overview : Api.Overview
overview =
    { aggregates = []
    , nodeCount = 0
    , edgeCount = 0
    , communityCount = 0
    , graphHash = "deadbeef"
    }


{-| Did an `/api/overview` body decode as an Overview? -}
decodeIsOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


suite : Test
suite =
    describe "Studio.Api"
        [ describe "probeSlices availability decision"
            [ test "a decoded Overview reports available" <|
                \_ ->
                    Api.isSliceProbeOk (Ok overview)
                        |> Expect.equal True
            , test "an HTTP failure reports not available" <|
                \_ ->
                    Api.isSliceProbeOk (Err Http.NetworkError)
                        |> Expect.equal False
            , test "a malformed body reports not available" <|
                \_ ->
                    Api.isSliceProbeOk (Err (Http.BadBody "bad"))
                        |> Expect.equal False
            ]
        , describe "overview body validation"
            [ test "an empty overview body does not decode" <|
                \_ ->
                    D.decodeString Api.overviewDecoder ""
                        |> decodeIsOk
                        |> Expect.equal False
            , test "an overview body missing required fields does not decode" <|
                \_ ->
                    D.decodeString Api.overviewDecoder "{ \"node_count\": 1 }"
                        |> decodeIsOk
                        |> Expect.equal False
            , test "a valid overview body decodes" <|
                \_ ->
                    D.decodeString Api.overviewDecoder "{ \"node_count\": 1, \"edge_count\": 2, \"community_count\": 3, \"graph_hash\": \"abc\", \"community_aggregates\": [] }"
                        |> decodeIsOk
                        |> Expect.equal True
            ]
        ]
