module EditTest exposing (suite)

{-| studio-graph-editing: validation table, inverse∘apply = identity,
undo/redo zipper, cypher translation, log round-trip.
-}

import Dict
import Expect
import Fixtures exposing (mkEdge, mkNode)
import Studio.Data.Graph as Graph
import Studio.Edit as Edit
import Test exposing (Test, describe, test)


g : Graph.Graph
g =
    Fixtures.chainGraph


expectInverseRestores : Edit.EditIntent -> Expect.Expectation
expectInverseRestores intent =
    Edit.applyIntent (Edit.inverse intent) (Edit.applyIntent intent g)
        |> Expect.all
            [ \g2 -> g2.nodes |> Expect.equal g.nodes
            , \g2 -> g2.edges |> Expect.equal g.edges
            ]


suite : Test
suite =
    describe "Studio.Edit"
        [ describe "validation"
            [ test "empty label is rejected client-side" <|
                \_ ->
                    Edit.validate g (Edit.Relabel "mod_Auth" "Auth" "  ")
                        |> Expect.err
            , test "unknown node is rejected" <|
                \_ ->
                    Edit.validate g (Edit.Relabel "ghost" "x" "y")
                        |> Expect.err
            , test "duplicate node id is rejected" <|
                \_ ->
                    Edit.validate g (Edit.CreateNode (mkNode "mod_Auth" "dup" 483))
                        |> Expect.err
            , test "edge to unknown target is rejected" <|
                \_ ->
                    Edit.validate g (Edit.CreateEdge (mkEdge "mod_Auth" "ghost" "calls"))
                        |> Expect.err
            , test "valid relabel passes" <|
                \_ ->
                    Edit.validate g (Edit.Relabel "mod_Auth" "Auth" "Authentication")
                        |> Expect.ok
            ]
        , describe "inverse ∘ apply = identity"
            [ test "relabel" <|
                \_ -> expectInverseRestores (Edit.Relabel "mod_Auth" "Auth" "Renamed")
            , test "retype" <|
                \_ -> expectInverseRestores (Edit.Retype "mod_Auth" (Just "Function") (Just "Module"))
            , test "create edge" <|
                \_ -> expectInverseRestores (Edit.CreateEdge (mkEdge "mod_Auth" "doc_readme" "references"))
            , test "delete edge" <|
                \_ -> expectInverseRestores (Edit.DeleteEdge (mkEdge "mod_Auth" "fn_verify" "calls"))
            , test "delete node restores its incident edges (hub-deletion undo)" <|
                \_ ->
                    expectInverseRestores
                        (Edit.DeleteNode (mkNode "fn_verify" "verifyToken" 483)
                            (Graph.incidentEdges g "fn_verify")
                        )
            ]
        , describe "undo/redo zipper"
            [ test "undo yields the inverse, redo re-applies" <|
                \_ ->
                    let
                        intent =
                            Edit.Relabel "mod_Auth" "Auth" "Renamed"

                        stack =
                            Edit.pushDone intent Edit.emptyStack
                    in
                    case Edit.undo stack of
                        Nothing ->
                            Expect.fail "undo available after push"

                        Just ( inv, stack2 ) ->
                            case Edit.redo stack2 of
                                Nothing ->
                                    Expect.fail "redo available after undo"

                                Just ( again, _ ) ->
                                    ( inv, again )
                                        |> Expect.equal
                                            ( Edit.Relabel "mod_Auth" "Renamed" "Auth", intent )
            , test "new edit clears the redo lane" <|
                \_ ->
                    let
                        a =
                            Edit.Relabel "mod_Auth" "Auth" "A"

                        b =
                            Edit.Relabel "mod_Auth" "A" "B"

                        afterUndo =
                            Edit.pushDone a Edit.emptyStack
                                |> Edit.undo
                                |> Maybe.map Tuple.second
                                |> Maybe.withDefault Edit.emptyStack
                    in
                    Edit.pushDone b afterUndo
                        |> .undone
                        |> Expect.equalLists []
            ]
        , describe "cypher translation (cypher-mutation subset)"
            [ test "relabel is MATCH + SET" <|
                \_ ->
                    Edit.toCypher (Edit.Relabel "mod_Auth" "Auth" "Renamed")
                        |> Expect.equal
                            "MATCH (n {id: 'mod_Auth'}) SET n.label = 'Renamed'"
            , test "delete node is DETACH DELETE" <|
                \_ ->
                    Edit.toCypher (Edit.DeleteNode (mkNode "mod_Auth" "Auth" 483) [])
                        |> Expect.equal "MATCH (n {id: 'mod_Auth'}) DETACH DELETE n"
            , test "single quotes are escaped" <|
                \_ ->
                    Edit.toCypher (Edit.Relabel "n1" "old" "it's quoted")
                        |> String.contains "\\'"
                        |> Expect.equal True
            ]
        , describe "edit-log persistence"
            [ test "encode/decode round-trips every intent shape" <|
                \_ ->
                    let
                        log =
                            [ Edit.Relabel "a" "x" "y"
                            , Edit.Retype "a" Nothing (Just "Module")
                            , Edit.CreateNode (mkNode "new1" "New" 483)
                            , Edit.DeleteNode (mkNode "fn_verify" "verifyToken" 483)
                                [ mkEdge "mod_Auth" "fn_verify" "calls" ]
                            , Edit.CreateEdge (mkEdge "a" "b" "calls")
                            , Edit.DeleteEdge (mkEdge "a" "b" "calls")
                            ]
                    in
                    Edit.decodeLog (Edit.encodeLog log)
                        |> Expect.equal (Ok log)
            ]
        ]
