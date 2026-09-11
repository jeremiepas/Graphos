module ScopeTest exposing (suite)

{-| studio-subgraph-extract: scope algebra, boundary rule, canonical encoding. -}

import Expect
import Fixtures
import Json.Decode as D
import Json.Encode as E
import Set
import Studio.Groups as Groups
import Studio.Scope as Scope
import Test exposing (Test, describe, test)


g =
    Fixtures.chainGraph


suite : Test
suite =
    describe "Studio.Scope"
        [ describe "algebra"
            [ test "community scope collects its members" <|
                \_ ->
                    Scope.addCommunity g 483 Scope.empty
                        |> Expect.equal (Set.fromList [ "mod_Auth", "fn_verify" ])
            , test "scopes union (community ∪ neighborhood)" <|
                \_ ->
                    Scope.empty
                        |> Scope.addCommunity g 483
                        |> Scope.addNeighborhood g "doc_readme" 1
                        |> Expect.equal
                            (Set.fromList [ "mod_Auth", "fn_verify", "mod_Config", "doc_readme" ])
            , test "group scope collects matching nodes" <|
                \_ ->
                    Scope.addGroup g (Groups.newGroup "cfg" "config" "#fff") Scope.empty
                        |> Expect.equal (Set.fromList [ "mod_Config" ])
            ]
        , describe "boundary rule"
            [ test "without boundary: only both-endpoint edges" <|
                \_ ->
                    Scope.scopeEdges g False (Set.fromList [ "mod_Auth", "fn_verify" ])
                        |> .edges
                        |> List.map .id
                        |> Expect.equalLists [ "mod_Auth->fn_verify" ]
            , test "with boundary: one-hop edges + their outside endpoints" <|
                \_ ->
                    let
                        result =
                            Scope.scopeEdges g True (Set.fromList [ "mod_Auth", "fn_verify" ])
                    in
                    Expect.all
                        [ \r -> List.length r.edges |> Expect.equal 2
                        , \r -> r.boundaryNodes |> Expect.equal (Set.fromList [ "mod_Config" ])
                        ]
                        result
            , test "counts include boundary nodes" <|
                \_ ->
                    Scope.counts g True (Set.fromList [ "mod_Auth", "fn_verify" ])
                        |> Expect.equal { nodes = 3, edges = 2 }
            ]
        , describe "canonical export (json-graph-web-view shape)"
            [ test "emits vis-network nodes/edges with relation" <|
                \_ ->
                    let
                        doc =
                            Scope.encodeCanonical
                                { title = "auth-core", boundary = False, bakeGroups = False }
                                g
                                []
                                (Set.fromList [ "mod_Auth", "fn_verify" ])

                        decoded =
                            D.decodeValue
                                (D.map3 (\t ns es -> ( t, ns, es ))
                                    (D.field "title" D.string)
                                    (D.field "nodes" (D.list (D.field "id" D.string)))
                                    (D.field "edges"
                                        (D.list
                                            (D.map2 Tuple.pair
                                                (D.field "from" D.string)
                                                (D.field "relation" D.string)
                                            )
                                        )
                                    )
                                )
                                doc
                    in
                    case decoded of
                        Err e ->
                            Expect.fail (D.errorToString e)

                        Ok ( title, nodeIds, edges ) ->
                            Expect.all
                                [ \_ -> title |> Expect.equal "auth-core"
                                , \_ -> List.sort nodeIds |> Expect.equalLists [ "fn_verify", "mod_Auth" ]
                                , \_ -> edges |> Expect.equalLists [ ( "mod_Auth", "calls" ) ]
                                ]
                                ()
            , test "bake group colors: matching node carries the group color" <|
                \_ ->
                    let
                        doc =
                            Scope.encodeCanonical
                                { title = "t", boundary = False, bakeGroups = True }
                                g
                                [ Groups.newGroup "auth" "auth" "#ff5900" ]
                                (Set.fromList [ "mod_Auth", "doc_readme" ])

                        colors =
                            D.decodeValue
                                (D.field "nodes"
                                    (D.list (D.map2 Tuple.pair (D.field "id" D.string) (D.field "color" D.string)))
                                )
                                doc
                                |> Result.withDefault []
                                |> List.filter (\( id, _ ) -> id == "mod_Auth")
                    in
                    colors |> Expect.equalLists [ ( "mod_Auth", "#ff5900" ) ]
            ]
        ]
