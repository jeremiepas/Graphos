module Studio.DialogFocus exposing
    ( DialogKey(..)
    , Key
    , ReturnTarget
    , interpretKey
    , nextIndex
    , prevIndex
    , recordReturn
    , returnTarget
    , tabStops
    , wrapIndex
    )

{-| Pure focus-management model for studio dialogs (`studio-design-system`).

A modal dialog must be keyboard-complete: `Tab` and `Shift+Tab` stay trapped
inside it, `Enter` confirms and `Escape` cancels, and focus returns to the
invoking control when the dialog closes. The pure arithmetic here — which stop
focus moves to under a `Tab` / `Shift+Tab` press — lives in Elm and is unit
tested; the browser glue (`public/ports.js`) only ever _moves_ the computed
index, never decides it.

-}

-- KEY INTERPRETATION


{-| The outcome of a key press inside a dialog.
-}
type DialogKey
    = Confirmed
    | Cancelled
    | Ignored


{-| A key press inside a dialog. `typing` is true when focus sits on a text
entry widget (input / textarea / select): there only `Escape` dismisses and
`Enter` is left for the user's characters rather than auto-confirming.
-}
type alias Key =
    { key : String
    , typing : Bool
    }


{-| Decode one key press into a dialog action. Typing input short-circuits to
`Ignored` except for `Escape`, which always cancels.
-}
interpretKey : Key -> DialogKey
interpretKey k =
    if k.typing then
        if k.key == "Escape" then
            Cancelled

        else
            Ignored

    else
        case k.key of
            "Enter" ->
                Confirmed

            "Escape" ->
                Cancelled

            _ ->
                Ignored



-- FOCUS TRAP ARITHMETIC


{-| The focusable stops inside a dialog, in tab order, as CSS selectors. The
dialog container itself is first so focus lands there on open, then each action
button in turn.
-}
tabStops : List String
tabStops =
    [ "#studio-dialog"
    , "#studio-dialog .btn-cancel"
    , "#studio-dialog .btn-confirm"
    ]


{-| Wrap an index into `[0, count)`. `modBy` returns a result whose sign follows
the divisor, so this is correct for negative indices too.
-}
wrapIndex : Int -> Int -> Int
wrapIndex count index =
    if count <= 0 then
        0

    else
        modBy count index


{-| The next stop after `index`, wrapping around at the end.
-}
nextIndex : Int -> Int
nextIndex index =
    wrapIndex (List.length tabStops) (index + 1)


{-| The previous stop before `index`, wrapping around at the start.
-}
prevIndex : Int -> Int
prevIndex index =
    wrapIndex (List.length tabStops) (index - 1)



-- FOCUS RETURN


{-| The control that should receive focus again when the dialog closes.
-}
type ReturnTarget
    = ReturnTarget String


{-| Record the currently-focused control (by id or selector) as a return target.
-}
recordReturn : String -> ReturnTarget
recordReturn selector =
    ReturnTarget selector


{-| The recorded id / selector.
-}
returnTarget : ReturnTarget -> String
returnTarget (ReturnTarget selector) =
    selector
