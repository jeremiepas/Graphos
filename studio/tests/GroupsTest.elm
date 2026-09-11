module GroupsTest exposing (suite)

{-| studio-groups: local evaluation subset, first-match-wins precedence,
isolation, group-set JSON round-trip.
-}

import Dict
import Expect
import Fixtures
import Studio.Groups as Groups
import Test exposing (Test, describe, test)


g =
    Fixtures.chainGraph


suite : Test
suite =
    describe "Studio.Groups"
        [ describe "local evaluation (label / path / kind)"
            [ test "matches label case-insensitively" <|
                \_ ->
                    Dict.get "fn_verify" g.nodes
                        |> Maybe.map (Groups.matchLocal "VERIFY")
                        |> Expect.equal (Just True)
            , test "matches source path" <|
                \_ ->
                    Dict.get "mod_Auth" g.nodes
                        |> Maybe.map (Groups.matchLocal "src/mod_auth")
                        |> Expect.equal (Just True)
            , test "matches kind" <|
                \_ ->
                    Dict.get "mod_Auth" g.nodes
                        |> Maybe.map (Groups.matchLocal "function")
                        |> Expect.equal (Just True)
            , test "all terms must match" <|
                \_ ->
                    Dict.get "mod_Auth" g.nodes
                        |> Maybe.map (Groups.matchLocal "auth nonexistent")
                        |> Expect.equal (Just False)
            , test "empty query matches nothing" <|
                \_ ->
                    Dict.get "mod_Auth" g.nodes
                        |> Maybe.map (Groups.matchLocal "   ")
                        |> Expect.equal (Just False)
            ]
        , describe "first-match-wins precedence"
            [ test "node matching two groups colors by the earlier one" <|
                \_ ->
                    let
                        groups =
                            [ Groups.newGroup "first" "verify" "#111111"
                            , Groups.newGroup "second" "verify" "#222222"
                            ]
                    in
                    Groups.memberships groups g
                        |> Dict.get "fn_verify"
                        |> Expect.equal (Just 0)
            , test "reordering flips the winner" <|
                \_ ->
                    let
                        groups =
                            [ Groups.newGroup "second" "verify" "#222222"
                            , Groups.newGroup "first" "verify" "#111111"
                            ]
                    in
                    Groups.memberships groups g
                        |> Dict.get "fn_verify"
                        |> Expect.equal (Just 0)
            ]
        , describe "isolation"
            [ test "no isolated group → no visibility map" <|
                \_ ->
                    Groups.visibleUnderIsolation [ Groups.newGroup "a" "auth" "#fff" ] g
                        |> Expect.equal Nothing
            , test "isolated group: only members visible" <|
                \_ ->
                    let
                        grp =
                            Groups.newGroup "auth" "auth" "#fff"

                        vis =
                            Groups.visibleUnderIsolation [ { grp | isolated = True } ] g
                                |> Maybe.withDefault Dict.empty
                    in
                    ( Dict.get "mod_Auth" vis, Dict.get "doc_readme" vis )
                        |> Expect.equal ( Just True, Just False )
            ]
        , describe "group-set JSON"
            [ test "encode/decode round-trips order, colors and flags" <|
                \_ ->
                    let
                        grp =
                            Groups.newGroup "tests" "spec test" "#00ff00"

                        groups =
                            [ Groups.newGroup "auth" "auth token" "#ff5900"
                            , { grp | hidden = True, isolated = True }
                            ]
                    in
                    Groups.decodeSet (Groups.encodeSet groups)
                        |> Expect.equal (Ok groups)
            ]
        ]
