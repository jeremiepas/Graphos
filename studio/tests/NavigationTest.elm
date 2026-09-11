module NavigationTest exposing (suite)

{-| Asserts the invariants proved by the viewer-navigation Lean model
(openspec/changes/viewer-navigation/lean/) on the Elm implementation:
escape descends and terminates, breadcrumb/escape agreement, codec
round-trip, sanitize degradation, push/replace classification.
-}

import Expect
import Fuzz exposing (Fuzzer)
import Studio.Navigation as Navi exposing (Position(..))
import Test exposing (Test, describe, fuzz, test)


nonEmptyString : Fuzzer String
nonEmptyString =
    Fuzz.map2 String.cons Fuzz.char Fuzz.string


positionFuzzer : Fuzzer Position
positionFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Overview
        , Fuzz.map Community (Fuzz.intRange 0 100000)
        , Fuzz.map2 Node (Fuzz.intRange 0 100000) nonEmptyString
        ]


stateFuzzer : Fuzzer Navi.ViewState
stateFuzzer =
    Fuzz.map3
        (\pos hops facets -> { pos = pos, tuning = { hops = hops, facets = facets } })
        positionFuzzer
        (Fuzz.intRange 1 6)
        (Fuzz.listOfLengthBetween 0 5 nonEmptyString)


suite : Test
suite =
    describe "Studio.Navigation (Lean model invariants)"
        [ describe "URL codec"
            [ fuzz stateFuzzer "decode_encode: decodeHash (encodeHash s) == Just s" <|
                \state ->
                    Navi.decodeHash (Navi.encodeHash state)
                        |> Expect.equal (Just state)
            , test "malformed hash decodes to Nothing, never crashes" <|
                \_ ->
                    [ "#garbage", "#/c/notanint", "#/n/12", "", "#/o?nope" ]
                        |> List.map Navi.decodeHash
                        |> Expect.equalLists [ Nothing, Nothing, Nothing, Nothing, Nothing ]
            ]
        , describe "escape"
            [ fuzz positionFuzzer "escape_rank_lt: strictly descends for non-Overview" <|
                \pos ->
                    if pos == Overview then
                        Navi.escape pos |> Expect.equal Overview

                    else
                        Navi.rank (Navi.escape pos)
                            |> Expect.lessThan (Navi.rank pos)
            , fuzz positionFuzzer "escape_escape: two escapes reach Overview from anywhere" <|
                \pos ->
                    Navi.escape (Navi.escape pos) |> Expect.equal Overview
            ]
        , describe "breadcrumb"
            [ fuzz positionFuzzer "≤ 3 segments" <|
                \pos ->
                    List.length (Navi.breadcrumb pos) |> Expect.atMost 3
            , fuzz positionFuzzer "starts at Overview" <|
                \pos ->
                    List.head (Navi.breadcrumb pos) |> Expect.equal (Just Overview)
            , fuzz positionFuzzer "last segment is the current position" <|
                \pos ->
                    List.reverse (Navi.breadcrumb pos)
                        |> List.head
                        |> Expect.equal (Just pos)
            , fuzz positionFuzzer "breadcrumb_escape: escaped trail == dropLast trail" <|
                \pos ->
                    if pos == Overview then
                        Expect.pass

                    else
                        Navi.breadcrumb (Navi.escape pos)
                            |> Expect.equal
                                (Navi.breadcrumb pos
                                    |> List.take (List.length (Navi.breadcrumb pos) - 1)
                                )
            ]
        , describe "sanitize"
            [ fuzz stateFuzzer "invalid position degrades to Overview keeping tuning" <|
                \state ->
                    let
                        sanitized =
                            Navi.sanitize (\_ -> False) { state | pos = Community 999 }
                    in
                    ( sanitized.pos, sanitized.tuning )
                        |> Expect.equal ( Overview, state.tuning )
            , fuzz stateFuzzer "valid position is untouched" <|
                \state ->
                    Navi.sanitize (\_ -> True) state |> Expect.equal state
            ]
        , describe "push/replace classification"
            [ test "tuning-only change does not push (steps_nonpos_back precondition)" <|
                \_ ->
                    let
                        base =
                            { pos = Community 483, tuning = { hops = 2, facets = [] } }
                    in
                    Navi.isPositionChange base { base | tuning = { hops = 4, facets = [ "doc" ] } }
                        |> Expect.equal False
            , test "position change pushes" <|
                \_ ->
                    let
                        base =
                            { pos = Community 483, tuning = { hops = 2, facets = [] } }
                    in
                    Navi.isPositionChange base { base | pos = Node 483 "mod_Auth" }
                        |> Expect.equal True
            ]
        ]
