module DialogFocusTest exposing (suite)

{-| Asserts the pure focus-management model (`Studio.DialogFocus`) that keeps a
studio dialog keyboard-complete: key interpretation, focus-trap wrap arithmetic,
and focus-return round-trip. -}

import Expect
import Fuzz exposing (Fuzzer)
import Studio.DialogFocus as DF
import Test exposing (Test, describe, fuzz, test)


noType : String -> DF.Key
noType key =
    { key = key, typing = False }


withType : String -> DF.Key
withType key =
    { key = key, typing = True }


stopCount : Int
stopCount =
    List.length DF.tabStops


suite : Test
suite =
    describe "Studio.DialogFocus"
        [ describe "interpretKey"
            [ test "Enter confirms when not typing" <|
                \_ -> DF.interpretKey (noType "Enter") |> Expect.equal DF.Confirmed
            , test "Escape cancels when not typing" <|
                \_ -> DF.interpretKey (noType "Escape") |> Expect.equal DF.Cancelled
            , test "other keys are ignored when not typing" <|
                \_ -> DF.interpretKey (noType "Tab") |> Expect.equal DF.Ignored
            , test "typing Enter is ignored (user is mid-typing)" <|
                \_ -> DF.interpretKey (withType "Enter") |> Expect.equal DF.Ignored
            , test "typing Tab is ignored" <|
                \_ -> DF.interpretKey (withType "Tab") |> Expect.equal DF.Ignored
            , test "typing Escape still cancels" <|
                \_ -> DF.interpretKey (withType "Escape") |> Expect.equal DF.Cancelled
            ]
        , describe "tabStops"
            [ test "has the dialog container first, then cancel, then confirm" <|
                \_ ->
                    [ "#studio-dialog", "#studio-dialog .btn-cancel", "#studio-dialog .btn-confirm" ]
                        |> Expect.equal DF.tabStops
            , test "at least one stop so the trap is never empty" <|
                \_ ->
                    DF.tabStops
                        |> List.length
                        |> Expect.atLeast 1
            ]
        , describe "focus-trap wrap"
            [ test "wrapIndex keeps an in-range index unchanged" <|
                \_ -> DF.wrapIndex 3 1 |> Expect.equal 1
            , test "nextIndex wraps past the last stop to the first" <|
                \_ -> DF.nextIndex (stopCount - 1) |> Expect.equal 0
            , test "prevIndex wraps before the first stop to the last" <|
                \_ -> DF.prevIndex 0 |> Expect.equal (stopCount - 1)
            , test "wrapIndex handles negative indices via modBy sign of divisor" <|
                \_ -> DF.wrapIndex 3 (-1) |> Expect.equal 2
            , test "wrapIndex clamps a zero count to 0" <|
                \_ -> DF.wrapIndex 0 5 |> Expect.equal 0
            , fuzz (Fuzz.intRange 0 1000) "nextIndex (prevIndex i) returns i wrapped" <|
                \i ->
                    DF.nextIndex (DF.prevIndex (DF.wrapIndex stopCount i))
                        |> Expect.equal (DF.wrapIndex stopCount i)
            ]
        , describe "focus return"
            [ test "recordReturn then returnTarget round-trips the selector" <|
                \_ ->
                    DF.returnTarget (DF.recordReturn "#studio-dialog .btn-cancel")
                        |> Expect.equal "#studio-dialog .btn-cancel"
            , test "empty selector round-trips" <|
                \_ -> DF.returnTarget (DF.recordReturn "") |> Expect.equal ""
            ]
        ]
