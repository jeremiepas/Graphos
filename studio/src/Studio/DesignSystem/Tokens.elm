module Studio.DesignSystem.Tokens exposing
    ( Theme(..)
    , defaultGroupColor
    , Tokens
    , harmonize
    , stylesheet
    , themeFromString
    , themeToString
    , tokens
    )

{-| The single source of truth for the studio's visual language
(`studio-design-system`): every color, size and radius in the app comes from
this module — literal values anywhere else fail the lint gate. The stylesheet
is generated from the token record, so tokens and CSS cannot drift.
-}

-- THEME


type Theme
    = Light
    | Dark


{-| Default color offered for a newly created group. -}
defaultGroupColor : String
defaultGroupColor =
    "#e8833a"


themeToString : Theme -> String
themeToString t =
    case t of
        Light ->
            "light"

        Dark ->
            "dark"


themeFromString : String -> Theme
themeFromString s =
    if s == "dark" then
        Dark

    else
        Light



-- TOKENS


type alias Tokens =
    { -- color roles
      bg : String
    , surface : String
    , surfaceAlt : String
    , text : String
    , textMuted : String
    , accent : String
    , accentText : String
    , danger : String
    , warning : String
    , success : String
    , border : String
    , focus : String

    -- graph canvas roles
    , canvasBg : String
    , canvasEdge : String
    , canvasLabel : String
    , canvasDim : String

    -- type scale (px)
    , fontXs : Int
    , fontSm : Int
    , fontMd : Int
    , fontLg : Int
    , fontXl : Int

    -- spacing scale (px)
    , s1 : Int
    , s2 : Int
    , s3 : Int
    , s4 : Int
    , s6 : Int
    , s8 : Int

    -- radii / elevation
    , radiusSm : Int
    , radiusMd : Int
    , shadow : String
    }


tokens : Theme -> Tokens
tokens theme =
    case theme of
        Light ->
            { bg = "#f6f7f9"
            , surface = "#ffffff"
            , surfaceAlt = "#eef0f3"
            , text = "#1a1d21"
            , textMuted = "#5c6470"
            , accent = "#2563eb"
            , accentText = "#ffffff"
            , danger = "#dc2626"
            , warning = "#d97706"
            , success = "#16a34a"
            , border = "#d8dce2"
            , focus = "#93b4f5"
            , canvasBg = "#fdfdfe"
            , canvasEdge = "#c3c9d2"
            , canvasLabel = "#33383f"
            , canvasDim = "#e3e6ea"
            , fontXs = 11
            , fontSm = 12
            , fontMd = 14
            , fontLg = 16
            , fontXl = 20
            , s1 = 4
            , s2 = 8
            , s3 = 12
            , s4 = 16
            , s6 = 24
            , s8 = 32
            , radiusSm = 4
            , radiusMd = 8
            , shadow = "0 4px 16px rgba(16,20,28,0.14)"
            }

        Dark ->
            { bg = "#12151a"
            , surface = "#1b1f26"
            , surfaceAlt = "#242933"
            , text = "#e6e9ee"
            , textMuted = "#9aa3b0"
            , accent = "#5b8def"
            , accentText = "#0d1117"
            , danger = "#f0555f"
            , warning = "#e5a03c"
            , success = "#3fbf6f"
            , border = "#323945"
            , focus = "#3d5a99"
            , canvasBg = "#0e1116"
            , canvasEdge = "#3a4150"
            , canvasLabel = "#c9cfd8"
            , canvasDim = "#232833"
            , fontXs = 11
            , fontSm = 12
            , fontMd = 14
            , fontLg = 16
            , fontXl = 20
            , s1 = 4
            , s2 = 8
            , s3 = 12
            , s4 = 16
            , s6 = 24
            , s8 = 32
            , radiusSm = 4
            , radiusMd = 8
            , shadow = "0 4px 16px rgba(0,0,0,0.45)"
            }



-- COMMUNITY COLOR HARMONIZATION
--
-- Community colors arrive as arbitrary hex/hsl from the export; clamp their
-- lightness into a band that keeps contrast against the theme canvas.


harmonize : Theme -> String -> String
harmonize theme color =
    case parseHex color of
        Nothing ->
            -- hsl()/named colors pass through — the synthesized palette is
            -- already in-band.
            color

        Just ( r, g, b ) ->
            let
                ( h, s, l ) =
                    rgbToHsl r g b

                clamped =
                    case theme of
                        Dark ->
                            clamp 0.45 0.72 l

                        Light ->
                            clamp 0.3 0.62 l
            in
            hslString h s clamped


parseHex : String -> Maybe ( Int, Int, Int )
parseHex color =
    let
        hex =
            if String.startsWith "#" color then
                String.dropLeft 1 color

            else
                color

        channel i =
            hexPair (String.slice (i * 2) (i * 2 + 2) hex)
    in
    if String.length hex == 6 then
        Maybe.map3 (\r g b -> ( r, g, b )) (channel 0) (channel 1) (channel 2)

    else
        Nothing


hexPair : String -> Maybe Int
hexPair s =
    let
        digit c =
            String.indexes (String.fromChar (Char.toLower c)) "0123456789abcdef"
                |> List.head
    in
    case String.toList s of
        [ a, b ] ->
            Maybe.map2 (\x y -> x * 16 + y) (digit a) (digit b)

        _ ->
            Nothing


rgbToHsl : Int -> Int -> Int -> ( Float, Float, Float )
rgbToHsl ri gi bi =
    let
        r =
            toFloat ri / 255

        g =
            toFloat gi / 255

        b =
            toFloat bi / 255

        maxC =
            max r (max g b)

        minC =
            min r (min g b)

        l =
            (maxC + minC) / 2

        d =
            maxC - minC

        s =
            if d == 0 then
                0

            else
                d / (1 - abs (2 * l - 1))

        h =
            if d == 0 then
                0

            else if maxC == r then
                60 * fmod ((g - b) / d) 6

            else if maxC == g then
                60 * ((b - r) / d + 2)

            else
                60 * ((r - g) / d + 4)
    in
    ( if h < 0 then
        h + 360

      else
        h
    , s
    , l
    )


fmod : Float -> Float -> Float
fmod a m =
    a - m * toFloat (floor (a / m))


hslString : Float -> Float -> Float -> String
hslString h s l =
    "hsl("
        ++ String.fromInt (round h)
        ++ ","
        ++ String.fromInt (round (s * 100))
        ++ "%,"
        ++ String.fromInt (round (l * 100))
        ++ "%)"



-- STYLESHEET (generated from tokens — the only CSS in the app)


px : Int -> String
px n =
    String.fromInt n ++ "px"


stylesheet : Theme -> String
stylesheet theme =
    let
        t =
            tokens theme

        rule selector body =
            selector ++ "{" ++ body ++ "}"
    in
    String.join "\n"
        [ rule "*" "box-sizing:border-box;margin:0"
        , rule "html,body,#app"
            ("height:100%;font-family:'Inter','Segoe UI',system-ui,sans-serif;font-size:"
                ++ px t.fontMd
                ++ ";background:"
                ++ t.bg
                ++ ";color:"
                ++ t.text
            )
        , rule ".studio-shell" "display:flex;flex-direction:column;height:100%"
        , rule ".studio-topbar"
            ("display:flex;align-items:center;gap:" ++ px t.s3 ++ ";padding:" ++ px t.s2 ++ " " ++ px t.s4 ++ ";background:" ++ t.surface ++ ";border-bottom:1px solid " ++ t.border)
        , rule ".studio-main" "display:flex;flex:1;min-height:0"
        , rule ".studio-sidebar"
            ("width:320px;min-width:320px;overflow-y:auto;background:" ++ t.surface ++ ";border-right:1px solid " ++ t.border ++ ";padding:" ++ px t.s3)
        , rule ".studio-canvas" ("flex:1;position:relative;background:" ++ t.canvasBg)
        , rule "#graph-canvas" "position:absolute;inset:0"
        , rule ".studio-detail"
            ("width:340px;min-width:340px;overflow-y:auto;background:" ++ t.surface ++ ";border-left:1px solid " ++ t.border ++ ";padding:" ++ px t.s3)

        -- breadcrumb
        , rule ".crumbs" ("display:flex;align-items:center;gap:" ++ px t.s1 ++ ";font-size:" ++ px t.fontSm ++ ";flex-wrap:nowrap;white-space:nowrap;overflow:hidden")
        , rule ".crumb-link" ("color:" ++ t.accent ++ ";cursor:pointer;background:none;border:none;font-size:" ++ px t.fontSm ++ ";padding:0")
        , rule ".crumb-link:hover" "text-decoration:underline"
        , rule ".crumb-current" ("color:" ++ t.text ++ ";max-width:260px;overflow:hidden;text-overflow:ellipsis")
        , rule ".crumb-sep" ("color:" ++ t.textMuted)

        -- components
        , rule ".btn"
            ("display:inline-flex;align-items:center;gap:" ++ px t.s1 ++ ";padding:" ++ px t.s1 ++ " " ++ px t.s3 ++ ";border-radius:" ++ px t.radiusSm ++ ";border:1px solid " ++ t.border ++ ";background:" ++ t.surfaceAlt ++ ";color:" ++ t.text ++ ";cursor:pointer;font-size:" ++ px t.fontSm)
        , rule ".btn:hover:not(:disabled)" ("border-color:" ++ t.accent)
        , rule ".btn:disabled" "opacity:0.45;cursor:not-allowed"
        , rule ".btn:focus-visible" ("outline:2px solid " ++ t.focus ++ ";outline-offset:1px")
        , rule ".btn-primary" ("background:" ++ t.accent ++ ";border-color:" ++ t.accent ++ ";color:" ++ t.accentText)
        , rule ".btn-danger" ("background:" ++ t.danger ++ ";border-color:" ++ t.danger ++ ";color:" ++ t.accentText)
        , rule ".input,.select"
            ("width:100%;padding:" ++ px t.s1 ++ " " ++ px t.s2 ++ ";border-radius:" ++ px t.radiusSm ++ ";border:1px solid " ++ t.border ++ ";background:" ++ t.bg ++ ";color:" ++ t.text ++ ";font-size:" ++ px t.fontSm)
        , rule ".input:focus,.select:focus" ("outline:2px solid " ++ t.focus ++ ";outline-offset:0")
        , rule ".slider" "width:100%"
        , rule ".panel"
            ("background:" ++ t.surface ++ ";border:1px solid " ++ t.border ++ ";border-radius:" ++ px t.radiusMd ++ ";padding:" ++ px t.s3 ++ ";margin-bottom:" ++ px t.s3)
        , rule ".panel-title"
            ("font-size:" ++ px t.fontSm ++ ";font-weight:600;text-transform:uppercase;letter-spacing:0.04em;color:" ++ t.textMuted ++ ";margin-bottom:" ++ px t.s2)
        , rule ".field-label" ("display:block;font-size:" ++ px t.fontXs ++ ";color:" ++ t.textMuted ++ ";margin:" ++ px t.s2 ++ " 0 " ++ px t.s1)
        , rule ".badge"
            ("display:inline-block;padding:0 " ++ px t.s2 ++ ";border-radius:" ++ px t.radiusMd ++ ";font-size:" ++ px t.fontXs ++ ";background:" ++ t.surfaceAlt ++ ";color:" ++ t.textMuted ++ ";border:1px solid " ++ t.border)
        , rule ".badge-accent" ("background:" ++ t.accent ++ ";color:" ++ t.accentText ++ ";border-color:" ++ t.accent)
        , rule ".badge-danger" ("background:" ++ t.danger ++ ";color:" ++ t.accentText ++ ";border-color:" ++ t.danger)

        -- dialog
        , rule ".dialog-backdrop"
            "position:fixed;inset:0;background:rgba(0,0,0,0.5);display:flex;align-items:center;justify-content:center;z-index:40"
        , rule ".dialog"
            ("background:" ++ t.surface ++ ";border:1px solid " ++ t.border ++ ";border-radius:" ++ px t.radiusMd ++ ";box-shadow:" ++ t.shadow ++ ";padding:" ++ px t.s4 ++ ";min-width:360px;max-width:520px")
        , rule ".dialog-title" ("font-size:" ++ px t.fontLg ++ ";font-weight:600;margin-bottom:" ++ px t.s3)
        , rule ".dialog-actions" ("display:flex;justify-content:flex-end;gap:" ++ px t.s2 ++ ";margin-top:" ++ px t.s4)

        -- toasts
        , rule ".toasts" ("position:fixed;bottom:" ++ px t.s4 ++ ";right:" ++ px t.s4 ++ ";display:flex;flex-direction:column;gap:" ++ px t.s2 ++ ";z-index:50")
        , rule ".toast"
            ("background:" ++ t.surface ++ ";border:1px solid " ++ t.border ++ ";border-left:3px solid " ++ t.accent ++ ";border-radius:" ++ px t.radiusSm ++ ";box-shadow:" ++ t.shadow ++ ";padding:" ++ px t.s2 ++ " " ++ px t.s3 ++ ";font-size:" ++ px t.fontSm ++ ";max-width:380px")
        , rule ".toast-error" ("border-left-color:" ++ t.danger)
        , rule ".toast-success" ("border-left-color:" ++ t.success)

        -- gallery
        , rule ".gallery"
            ("display:flex;flex-direction:column;gap:" ++ px t.s4 ++ ";padding:" ++ px t.s4 ++ ";overflow:auto;min-height:100%")
        , rule ".gallery-head"
            ("display:flex;align-items:baseline;gap:" ++ px t.s3 ++ ";margin-bottom:" ++ px t.s4)
        , rule ".gallery-head h2" ("font-size:" ++ px t.fontLg ++ ";margin:0")
        , rule ".gallery-grid"
            ("display:grid;grid-template-columns:repeat(auto-fill,minmax(320px,1fr));gap:" ++ px t.s3 ++ ";align-items:start")

        -- misc
        , rule ".empty-state" ("text-align:center;color:" ++ t.textMuted ++ ";padding:" ++ px t.s8 ++ " " ++ px t.s4)
        , rule ".muted" ("color:" ++ t.textMuted ++ ";font-size:" ++ px t.fontSm)
        , rule ".row" ("display:flex;align-items:center;gap:" ++ px t.s2)
        , rule ".spread" "display:flex;align-items:center;justify-content:space-between"
        , rule ".group-color-dot" ("width:12px;height:12px;border-radius:50%;display:inline-block;border:1px solid " ++ t.border)
        , rule ".drop-zone"
            ("border:2px dashed " ++ t.border ++ ";border-radius:" ++ px t.radiusMd ++ ";padding:" ++ px t.s8 ++ ";text-align:center;color:" ++ t.textMuted)
        , rule ".drop-zone-active" ("border-color:" ++ t.accent ++ ";color:" ++ t.accent)
        , rule ".kbd"
            ("font-family:monospace;font-size:" ++ px t.fontXs ++ ";background:" ++ t.surfaceAlt ++ ";border:1px solid " ++ t.border ++ ";border-radius:" ++ px t.radiusSm ++ ";padding:0 " ++ px t.s1)
        , rule ".list-item"
            ("padding:" ++ px t.s1 ++ " " ++ px t.s2 ++ ";border-radius:" ++ px t.radiusSm ++ ";cursor:pointer;font-size:" ++ px t.fontSm)
        , rule ".list-item:hover" ("background:" ++ t.surfaceAlt)
        ]
