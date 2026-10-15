module ContractTest exposing (suite)

{-| Graphos binary contract (AVI-1071): decode REAL graph.json output
produced by the graphos binary, not a hand-written fixture.

The `fixture` job in .github/workflows/haskell.yml runs the built binary
over the test corpus and uploads the resulting graph.json files as the
`graphos-fixture` artifact; the `studio-contract` job regenerates
ContractFixtures (codegen pre-step, since elm-test cannot read files) and
runs this suite. Each fixture must decode through the same decoder
`studio/src` uses for graph.json (Studio.Data.Graph.decoder) and carry
usable data, so a binary output change that breaks the studio fails CI.

The hand-written studio/tests/fixtures/graph.json stays for the offline
unit tests (GraphTest); this suite validates against actual binary output.
-}

import ContractFixtures
import Dict
import Expect
import Json.Decode as D
import Studio.Data.Graph as Graph
import Test exposing (Test, describe, test)


suite : Test
suite =
    Test.describe "graphos binary contract (real graph.json from the graphos-fixture artifact)"
        (if List.isEmpty ContractFixtures.fixtures then
            -- Placeholder state: no real fixtures embedded. CI regenerates
            -- ContractFixtures from the graphos-fixture artifact before this
            -- suite runs, so this branch is only taken in the committed
            -- placeholder (offline unit-test runs keep passing while the
            -- contract itself is explicitly reported as skipped).
            [ Test.skip <|
                Test.test "skipped: no graphos fixtures embedded (regenerate ContractFixtures via generate-contract-fixtures.sh)" <|
                    \_ -> Expect.fail "contract fixtures not embedded"
            ]
         else
            List.concatMap fixtureTests ContractFixtures.fixtures
        )


fixtureTests : ( String, String ) -> List Test
fixtureTests ( name, json ) =
    let
        decoded =
            D.decodeString Graph.decoder json
    in
    [ test (name ++ ": decodes through the studio graph decoder") <|
        \_ ->
            case decoded of
                Err err ->
                    Expect.fail (D.errorToString err)

                Ok g ->
                    Expect.all
                        [ \_ -> Graph.nodeCount g |> Expect.greaterThan 0
                        , \_ -> List.length g.aggregates |> Expect.greaterThan 0
                        ]
                        ()
    , test (name ++ ": every decoded node carries a usable source file") <|
        \_ ->
            case decoded of
                Err err ->
                    Expect.fail (D.errorToString err)

                Ok g ->
                    g.nodes
                        |> Dict.values
                        |> List.all (\n -> not (String.isEmpty n.sourceFile))
                        |> Expect.equal True
    , test (name ++ ": every decoded edge references decoded nodes") <|
        \_ ->
            case decoded of
                Err err ->
                    Expect.fail (D.errorToString err)

                Ok g ->
                    g.edges
                        |> Dict.values
                        |> List.all
                            (\e ->
                                (Dict.member e.source g.nodes)
                                    && (Dict.member e.target g.nodes)
                            )
                        |> Expect.equal True
    , test (name ++ ": every community id resolves to a label") <|
        \_ ->
            case decoded of
                Err err ->
                    Expect.fail (D.errorToString err)

                Ok g ->
                    g.aggregates
                        |> List.all
                            (\a ->
                                not (String.isEmpty (Graph.communityLabel g a.id))
                            )
                        |> Expect.equal True
    ]