module Studio.DesignSystem.Components exposing
    ( badge
    , btn
    , btnDanger
    , btnPrimary
    , dialog
    , emptyState
    , field
    , panel
    , selectInput
    , slider
    , textInput
    , toast
    )

{-| The studio component library (`studio-design-system`): interactive
elements compose these, never raw styled HTML. All styling comes from the
Tokens-generated stylesheet classes.
-}

import Html exposing (Html, button, div, input, label, option, select, span, text)
import Html.Attributes as A
import Html.Events as Ev
import Json.Decode as D



-- BUTTONS


btnBase : String -> { label : String, enabled : Bool, onPress : msg } -> Html msg
btnBase cls cfg =
    button
        [ A.class cls
        , A.disabled (not cfg.enabled)
        , Ev.onClick cfg.onPress
        ]
        [ text cfg.label ]


btn : { label : String, enabled : Bool, onPress : msg } -> Html msg
btn =
    btnBase "btn"


btnPrimary : { label : String, enabled : Bool, onPress : msg } -> Html msg
btnPrimary =
    btnBase "btn btn-primary"


btnDanger : { label : String, enabled : Bool, onPress : msg } -> Html msg
btnDanger =
    btnBase "btn btn-danger"



-- FORM ELEMENTS


field : String -> Html msg -> Html msg
field labelText control =
    div []
        [ label [ A.class "field-label" ] [ text labelText ]
        , control
        ]


textInput : { value : String, placeholder : String, onInput : String -> msg } -> Html msg
textInput cfg =
    input
        [ A.class "input"
        , A.type_ "text"
        , A.value cfg.value
        , A.placeholder cfg.placeholder
        , Ev.onInput cfg.onInput
        ]
        []


selectInput : { value : String, options : List ( String, String ), onSelect : String -> msg } -> Html msg
selectInput cfg =
    select
        [ A.class "select", Ev.onInput cfg.onSelect ]
        (List.map
            (\( v, lbl ) ->
                option [ A.value v, A.selected (v == cfg.value) ] [ text lbl ]
            )
            cfg.options
        )


slider : { min : Int, max : Int, value : Int, onChange : Int -> msg } -> Html msg
slider cfg =
    input
        [ A.class "slider"
        , A.type_ "range"
        , A.min (String.fromInt cfg.min)
        , A.max (String.fromInt cfg.max)
        , A.value (String.fromInt cfg.value)
        , Ev.onInput (\s -> cfg.onChange (Maybe.withDefault cfg.value (String.toInt s)))
        ]
        []



-- CONTAINERS


panel : String -> List (Html msg) -> Html msg
panel title children =
    div [ A.class "panel" ]
        (div [ A.class "panel-title" ] [ text title ] :: children)


badge : String -> String -> Html msg
badge variant content =
    span
        [ A.class
            (case variant of
                "accent" ->
                    "badge badge-accent"

                "danger" ->
                    "badge badge-danger"

                _ ->
                    "badge"
            )
        ]
        [ text content ]


emptyState : String -> Html msg
emptyState message =
    div [ A.class "empty-state" ] [ text message ]



-- DIALOG (Escape cancels, Enter confirms, backdrop click cancels)


dialog :
    { title : String
    , body : List (Html msg)
    , confirmLabel : String
    , onConfirm : msg
    , onCancel : msg
    , destructive : Bool
    }
    -> Html msg
dialog cfg =
    let
        keyHandler =
            Ev.on "keydown"
                (D.field "key" D.string
                    |> D.andThen
                        (\key ->
                            case key of
                                "Escape" ->
                                    D.succeed cfg.onCancel

                                "Enter" ->
                                    D.succeed cfg.onConfirm

                                _ ->
                                    D.fail "unhandled"
                        )
                )

        confirmBtn =
            if cfg.destructive then
                btnDanger { label = cfg.confirmLabel, enabled = True, onPress = cfg.onConfirm }

            else
                btnPrimary { label = cfg.confirmLabel, enabled = True, onPress = cfg.onConfirm }
    in
    div [ A.class "dialog-backdrop" ]
        [ div
            [ A.class "dialog"
            , A.tabindex 0
            , A.id "studio-dialog"
            , keyHandler
            ]
            (div [ A.class "dialog-title" ] [ text cfg.title ]
                :: cfg.body
                ++ [ div [ A.class "dialog-actions" ]
                        [ btn { label = "Cancel", enabled = True, onPress = cfg.onCancel }
                        , confirmBtn
                        ]
                   ]
            )
        ]



-- TOASTS


toast : { level : String, message : String } -> Html msg
toast cfg =
    div
        [ A.class
            (case cfg.level of
                "error" ->
                    "toast toast-error"

                "success" ->
                    "toast toast-success"

                _ ->
                    "toast"
            )
        ]
        [ text cfg.message ]
