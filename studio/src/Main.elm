port module Main exposing (main)

{-| Graphos Studio (`studio-app-shell`): one Model, one Msg, one update.
Effects live at the edges — HTTP (Studio.Api), files (elm/file), the renderer
and persistence ports. Navigation implements the `viewer-navigation` semantics
(Studio.Navigation, invariants verified by that change's Lean model).

Connected-mode edits are optimistic: applied locally at accept time, reverted
(inverse intent + pre-action stack snapshot) when the server rejects.

-}

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Dict exposing (Dict)
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Html, div, h1, h2, input, p, span, text)
import Html.Attributes as A
import Html.Events as Ev
import Json.Decode as D
import Json.Encode as E
import Set
import Studio.Api as Api
import Studio.Data.Graph as Graph exposing (Graph)
import Studio.Data.Source as Source
import Studio.DesignSystem.Components as UI
import Studio.DesignSystem.Tokens as Tokens exposing (Theme(..))
import Studio.DialogFocus as DF
import Studio.Edit as Edit
import Studio.Groups as Groups
import Studio.Navigation as Navi exposing (Position(..))
import Studio.Persist as Persist
import Studio.Scope as Scope
import Task
import Url exposing (Url)



-- PORTS


port toRenderer : E.Value -> Cmd msg


port fromRenderer : (D.Value -> msg) -> Sub msg


port persist : E.Value -> Cmd msg



-- ── Dialog focus ports (browser glue moves focus, Elm decides where) ───────


port openDialog : { stops : List String, index : Int } -> Cmd msg


port moveFocus : String -> Cmd msg


port restoreFocus : E.Value -> Cmd msg



-- CONSTANTS


{-| File-mode size guard lives in `Studio.Data.Source` (`studio-data-sources`):
refuse files above this rather than freezing the tab, and recommend connected
mode. Referenced here via `Source.sizeThresholdBytes`. -}



-- MODEL


type Source
    = NoGraph
    | FileMode String
    | Connected { origin : String, caps : Api.Capabilities }
    | Disconnected { origin : String }


type Dialog
    = ConfirmIntent Edit.EditIntent
    | OfferReplay (List Edit.EditIntent)
    | ExportScopeDialog { title : String, bake : Bool }
    | GalleryDemo { message : String }


type alias Toast =
    { id : Int, level : String, message : String }


type alias Model =
    { key : Nav.Key
    , theme : Theme
    , stored : Dict String D.Value
    , source : Source
    , graph : Graph
    , identity : Maybe String
    , caps : Api.Capabilities
    , probed : Int
    , overview : Maybe Source.Overview
    , nav : Navi.ViewState
    , pendingHash : Maybe String
    , suppressUrlMsg : Bool
    , groups : List Groups.Group
    , serverCounts : Dict Int Int
    , stack : Edit.UndoStack
    , log : List Edit.EditIntent
    , scope : Scope.Scope
    , scopeBoundary : Bool
    , scopePreview : Bool
    , dialog : Maybe Dialog
    , dialogStops : List String
    , focusIndex : Int
    , toasts : List Toast
    , toastSeq : Int
    , originInput : String
    , connecting : Bool
    , connectingOrigin : Maybe String
    , groupForm : { name : String, query : String, color : String }
    , editLabel : String
    , editKind : String
    , edgeTarget : String
    , edgeRelation : String
    , newNodeId : String
    , newNodeLabel : String
    , dropActive : Bool
    , helpOpen : Bool
    , galleryOpen : Bool
    , galleryTitle : String
    , gallerySelect : String
    , gallerySlider : Int
    }



-- INIT


type alias Flags =
    { stored : Dict String D.Value
    , osDark : Bool
    }


flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.map2 Flags
        (D.field "stored" (D.dict D.value))
        (D.field "osDark" D.bool)


init : D.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsValue url key =
    let
        flags =
            D.decodeValue flagsDecoder flagsValue
                |> Result.withDefault { stored = Dict.empty, osDark = False }

        theme =
            case Dict.get "graphos-studio:theme" flags.stored of
                Just v ->
                    D.decodeValue D.string v
                        |> Result.map Tokens.themeFromString
                        |> Result.withDefault (defaultTheme flags.osDark)

                Nothing ->
                    defaultTheme flags.osDark
    in
    ( { key = key
      , theme = theme
      , stored = flags.stored
      , source = NoGraph
       , graph = Graph.empty
       , identity = Nothing
       , caps = Api.noCapabilities
       , probed = 0
       , overview = Nothing
       , nav = Navi.initialState
      , pendingHash = url.fragment
      , suppressUrlMsg = False
      , groups = []
      , serverCounts = Dict.empty
      , stack = Edit.emptyStack
      , log = []
      , scope = Scope.empty
      , scopeBoundary = False
      , scopePreview = False
      , dialog = Nothing
      , dialogStops = []
      , focusIndex = 0
      , toasts = []
      , toastSeq = 0
       , originInput = "http://localhost:8080"
       , connecting = False
       , connectingOrigin = Nothing
      , groupForm = { name = "", query = "", color = Tokens.defaultGroupColor }
      , editLabel = ""
      , editKind = ""
      , edgeTarget = ""
      , edgeRelation = "references"
      , newNodeId = ""
      , newNodeLabel = ""
      , dropActive = False
      , helpOpen = False
      , galleryOpen = False
      , galleryTitle = ""
      , gallerySelect = "doc"
      , gallerySlider = 50
      }
    , Cmd.none
    )


defaultTheme : Bool -> Theme
defaultTheme osDark =
    if osDark then
        Dark

    else
        Light



-- MESSAGES


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | NavigateTo Position
    | SetHops Int
    | ToggleFacet String
    | KeyPressed { key : String, ctrl : Bool, shift : Bool, typing : Bool }
    | ToggleTheme
    | ToggleHelp
    | ToggleGallery
      -- gallery
    | GalleryInput String
    | GallerySelect String
    | GallerySlider Int
    | GalleryToast
    | GalleryNoop
    | OpenGalleryDemo
    | GalleryConfirmDialog
      -- source
    | PickFile
    | GotFile File
    | GotFileContent String String
    | DragEnter
    | DragLeave
    | GotDropFile File
    | OriginInput String
    | Connect
    | ProbedQuery Bool
    | ProbedSlices Bool
     | GotRemoteGraph ( String, Result String String )
     | GotOverview ( String, Result String String )
     | ConnectionLost
     | Retry
     | Discard
     | Disconnect
       -- groups
    | GroupFormName String
    | GroupFormQuery String
    | GroupFormColor String
    | AddGroup
    | DeleteGroup Int
    | MoveGroupUp Int
    | ToggleGroupHidden Int
    | ToggleGroupIsolated Int
    | GotServerCount Int (Result String Int)
    | ExportGroups
    | ImportGroups
    | GotGroupsFile File
    | GotGroupsContent String
      -- editing
    | EditLabelInput String
    | EditKindInput String
    | SubmitRelabel
    | SubmitRetype
    | EdgeTargetInput String
    | EdgeRelationInput String
    | SubmitCreateEdge
    | NewNodeIdInput String
    | NewNodeLabelInput String
    | SubmitCreateNode
    | RequestDeleteNode
    | RequestDeleteEdge String
    | ConfirmDialog
    | CancelDialog
    | DialogTab
    | DialogShiftTab
    | MutateResult Edit.EditIntent Edit.UndoStack (Result String ())
    | Undo
    | Redo
    | ExportGraph
    | ExportLog
      -- scope
    | ScopeAddCommunity
    | ScopeAddSelection
    | ScopeAddNeighborhood
    | ScopeAddGroup Int
    | ScopeClear
    | ScopeToggleBoundary
    | ScopeTogglePreview
    | OpenExportScope
    | ExportScopeTitle String
    | ExportScopeBake Bool
      -- misc
    | RendererEvent D.Value
    | DismissToast Int
    | NoOp



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlRequested req ->
            case req of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            if model.suppressUrlMsg then
                ( { model | suppressUrlMsg = False }, Cmd.none )

            else
                -- External position change (browser Back/Forward, hand-edited
                -- hash): same restore path as deep links.
                case url.fragment |> Maybe.andThen Navi.decodeHash of
                    Just state ->
                        let
                            m2 =
                                syncEditForms
                                    { model | nav = Navi.sanitize (validPos model.graph) state }
                        in
                        ( m2, renderCmd m2 )

                    Nothing ->
                        ( model, Cmd.none )

        NavigateTo pos ->
            changeNav { pos = pos, tuning = model.nav.tuning } model

        SetHops h ->
            changeNav { pos = model.nav.pos, tuning = withHops h model.nav.tuning } model

        ToggleFacet facet ->
            let
                tuning =
                    model.nav.tuning

                facets =
                    if List.member facet tuning.facets then
                        List.filter ((/=) facet) tuning.facets

                    else
                        facet :: tuning.facets
            in
            changeNav { pos = model.nav.pos, tuning = { tuning | facets = facets } } model

        KeyPressed k ->
            handleKey k model

        ToggleTheme ->
            let
                theme =
                    case model.theme of
                        Light ->
                            Dark

                        Dark ->
                            Light

                m2 =
                    { model | theme = theme }
            in
            ( m2
            , Cmd.batch
                [ persistValue "graphos-studio:theme" (E.string (Tokens.themeToString theme))
                , renderCmd m2
                ]
            )

        ToggleHelp ->
            ( { model | helpOpen = not model.helpOpen }, Cmd.none )

        ToggleGallery ->
            ( { model | galleryOpen = not model.galleryOpen }, Cmd.none )

        GalleryInput s ->
            ( { model | galleryTitle = s }, Cmd.none )

        GallerySelect s ->
            ( { model | gallerySelect = s }, Cmd.none )

        GallerySlider n ->
            ( { model | gallerySlider = n }, Cmd.none )

        GalleryToast ->
            addToast "success" "Gallery toast — this is a live, theme-styled toast." model

        OpenGalleryDemo ->
            armDialog (GalleryDemo { message = "This modal is keyboard-complete: Tab is trapped, Enter confirms, Escape cancels, and focus returns here on close." }) model

        GalleryConfirmDialog ->
            closeDialog model

        GalleryNoop ->
            ( model, Cmd.none )

        -- ── Source: file mode ──────────────────────────────────────────
        PickFile ->
            ( model, File.Select.file [ "application/json" ] GotFile )

        GotFile file ->
            loadFile file model

        DragEnter ->
            ( { model | dropActive = True }, Cmd.none )

        DragLeave ->
            ( { model | dropActive = False }, Cmd.none )

        GotDropFile file ->
            loadFile file { model | dropActive = False }

        GotFileContent name content ->
            graphLoaded (FileMode name) (Graph.fingerprint content) content model

        -- ── Source: connected mode ─────────────────────────────────────
        OriginInput s ->
            ( { model | originInput = s }, Cmd.none )

        Connect ->
            ( { model
                  | connecting = True
                  , probed = 0
                  , overview = Nothing
                  , connectingOrigin = Just model.originInput
                }
            , Cmd.batch
                [ Api.probeQuery model.originInput ProbedQuery
                , Api.probeSlices model.originInput ProbedSlices
                ]
            )

        ProbedQuery ok ->
            updateCaps (\c -> { c | query = ok }) model |> afterProbe

        ProbedSlices ok ->
            updateCaps (\c -> { c | slices = ok }) model |> afterProbe

        GotOverview ( origin, Err err ) ->
            addToast "error" err { model | connecting = False }

        GotOverview ( origin, Ok body ) ->
            case D.decodeString Source.overviewDecoder body of
                Ok ov ->
                    overviewLoaded origin ov { model | connecting = False }

                Err err ->
                    addToast "error"
                        ("Not an overview: " ++ shortDecodeError err)
                        { model | connecting = False }

        GotRemoteGraph ( origin, Err err ) ->
            addToast "error" err { model | connecting = False }

        GotRemoteGraph ( origin, Ok body ) ->
            if String.length body > Source.sizeThresholdBytes then
                addToast "error"
                    ("graph.json exceeds the studio threshold ("
                        ++ String.fromInt (Source.sizeThresholdBytes // (1024 * 1024))
                        ++ " MB) — this server needs the slice API (progressive-graph-interface)"
                    )
                    { model | connecting = False }

            else
                graphLoaded
                    (Connected { origin = origin, caps = model.caps })
                    (Graph.fingerprint body)
                    body
                    { model | connecting = False }

        ConnectionLost ->
            -- The origin just became unreachable mid-session. Preserve local
            -- work (groups/undo/log/scope/nav); a connection loss is not a data
            -- loss, so a Retry can resume from here (studio-data-sources 2.2).
            case model.source of
                Connected k ->
                    ( { model | source = Disconnected { origin = k.origin }, connecting = False }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Retry ->
            case model.source of
                Disconnected k ->
                    ( { model | connecting = True, probed = 0, connectingOrigin = Just k.origin }
                    , Cmd.batch
                        [ Api.probeQuery k.origin ProbedQuery
                        , Api.probeSlices k.origin ProbedSlices
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        Discard ->
            -- Explicit discard from the disconnected state: drop local work.
            case model.source of
                Disconnected _ ->
                    ( { model
                          | source = NoGraph
                          , graph = Graph.empty
                          , identity = Nothing
                          , nav = Navi.initialState
                          , suppressUrlMsg = True
                          , groups = []
                          , stack = Edit.emptyStack
                          , log = []
                          , scope = Scope.empty
                          , overview = Nothing
                          , connecting = False
                        }
                    , Nav.replaceUrl model.key (Navi.encodeHash Navi.initialState)
                    )

                _ ->
                    ( model, Cmd.none )

        Disconnect ->
            let
                m2 =
                    { model
                        | source = NoGraph
                        , graph = Graph.empty
                        , identity = Nothing
                        , nav = Navi.initialState
                        , suppressUrlMsg = True
                        , groups = []
                        , stack = Edit.emptyStack
                        , log = []
                        , scope = Scope.empty
                    }
            in
            ( m2, Nav.replaceUrl m2.key (Navi.encodeHash m2.nav) )

        -- ── Groups ─────────────────────────────────────────────────────
        GroupFormName s ->
            ( { model | groupForm = mapGroupForm (\f -> { f | name = s }) model }, Cmd.none )

        GroupFormQuery s ->
            ( { model | groupForm = mapGroupForm (\f -> { f | query = s }) model }, Cmd.none )

        GroupFormColor s ->
            ( { model | groupForm = mapGroupForm (\f -> { f | color = s }) model }, Cmd.none )

        AddGroup ->
            let
                f =
                    model.groupForm
            in
            if String.trim f.name == "" || String.trim f.query == "" then
                addToast "error" "Group needs a name and a query" model

            else
                groupsChanged
                    (model.groups ++ [ Groups.newGroup f.name f.query f.color ])
                    { model | groupForm = { name = "", query = "", color = f.color } }

        DeleteGroup i ->
            groupsChanged (removeAt i model.groups) model

        MoveGroupUp i ->
            groupsChanged (moveUp i model.groups) model

        ToggleGroupHidden i ->
            groupsChanged (mapAt i (\g -> { g | hidden = not g.hidden }) model.groups) model

        ToggleGroupIsolated i ->
            groupsChanged (mapAt i (\g -> { g | isolated = not g.isolated }) model.groups) model

        GotServerCount i result ->
            case result of
                Ok n ->
                    ( { model | serverCounts = Dict.insert i n model.serverCounts }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ExportGroups ->
            ( model
            , File.Download.string "groups.json" "application/json" (E.encode 2 (Groups.encodeSet model.groups))
            )

        ImportGroups ->
            ( model, File.Select.file [ "application/json" ] GotGroupsFile )

        GotGroupsFile file ->
            ( model, Task.perform GotGroupsContent (File.toString file) )

        GotGroupsContent content ->
            case
                D.decodeString D.value content
                    |> Result.mapError D.errorToString
                    |> Result.andThen (Groups.decodeSet >> Result.mapError D.errorToString)
            of
                Ok groups ->
                    groupsChanged groups model

                Err _ ->
                    addToast "error" "Not a group-set JSON document" model

        -- ── Editing ────────────────────────────────────────────────────
        EditLabelInput s ->
            ( { model | editLabel = s }, Cmd.none )

        EditKindInput s ->
            ( { model | editKind = s }, Cmd.none )

        SubmitRelabel ->
            withSelectedNode model
                (\node -> acceptIntent (Edit.Relabel node.id node.label model.editLabel) model)

        SubmitRetype ->
            withSelectedNode model
                (\node ->
                    acceptIntent
                        (Edit.Retype node.id
                            node.kind
                            (if String.trim model.editKind == "" then
                                Nothing

                             else
                                Just model.editKind
                            )
                        )
                        model
                )

        EdgeTargetInput s ->
            ( { model | edgeTarget = s }, Cmd.none )

        EdgeRelationInput s ->
            ( { model | edgeRelation = s }, Cmd.none )

        SubmitCreateEdge ->
            withSelectedNode model
                (\node ->
                    acceptIntent
                        (Edit.CreateEdge
                            { id = "studio:" ++ node.id ++ "->" ++ model.edgeTarget ++ ":" ++ model.edgeRelation
                            , source = node.id
                            , target = model.edgeTarget
                            , relation = model.edgeRelation
                            , weight = Nothing
                            , confidence = Nothing
                            , extra = Nothing
                            }
                        )
                        model
                )

        NewNodeIdInput s ->
            ( { model | newNodeId = s }, Cmd.none )

        NewNodeLabelInput s ->
            ( { model | newNodeLabel = s }, Cmd.none )

        SubmitCreateNode ->
            acceptIntent
                (Edit.CreateNode
                    { id = model.newNodeId
                    , label = model.newNodeLabel
                    , fileType = Graph.DocFile
                    , sourceFile = "studio://manual"
                    , lineStart = Nothing
                    , lineEnd = Nothing
                    , signature = Nothing
                    , communityId = currentCommunity model
                    , kind = Just "Manual"
                    , degree = Nothing
                    , isBridge = Nothing
                    , extra = Nothing
                    }
                )
                model

        RequestDeleteNode ->
            withSelectedNode model
                (\node ->
                    armDialog
                        (ConfirmIntent (Edit.DeleteNode node (Graph.incidentEdges model.graph node.id)))
                        model
                )

        RequestDeleteEdge edgeId ->
            case Dict.get edgeId model.graph.edges of
                Just e ->
                    armDialog (ConfirmIntent (Edit.DeleteEdge e)) model

                Nothing ->
                    ( model, Cmd.none )

        ConfirmDialog ->
            confirmDialog model

        CancelDialog ->
            closeDialog model

        DialogTab ->
            case model.dialog of
                Just _ ->
                    focusDialogAt model (DF.nextIndex model.focusIndex)

                Nothing ->
                    ( model, Cmd.none )

        DialogShiftTab ->
            case model.dialog of
                Just _ ->
                    focusDialogAt model (DF.prevIndex model.focusIndex)

                Nothing ->
                    ( model, Cmd.none )

        MutateResult intent prevStack result ->
            case result of
                Ok () ->
                    -- RestoreNode: node create succeeded; re-create its edges
                    -- now that the endpoint node exists server-side.
                    case ( intent, model.source ) of
                        ( Edit.RestoreNode _ edges, Connected c ) ->
                            ( model
                            , Cmd.batch
                                (List.map
                                    (\e ->
                                        Api.mutate c.origin
                                            (Edit.toCypher (Edit.CreateEdge e))
                                            (MutateResult (Edit.CreateEdge e) model.stack)
                                    )
                                    edges
                                )
                            )

                        _ ->
                            ( model, Cmd.none )

                Err serverErr ->
                    -- Optimistic apply → revert: inverse locally, restore the
                    -- pre-action stack (spec: "Server rejection reverts").
                    let
                        m2 =
                            { model
                                | graph = Edit.applyIntent (Edit.inverse intent) model.graph
                                , stack = prevStack
                            }
                    in
                    addToastWithCmd "error" ("Server rejected: " ++ serverErr) m2 (renderCmd m2)

        Undo ->
            case Edit.undo model.stack of
                Nothing ->
                    ( model, Cmd.none )

                Just ( inv, stack2 ) ->
                    applyAndSend inv model.stack { model | stack = stack2 }

        Redo ->
            case Edit.redo model.stack of
                Nothing ->
                    ( model, Cmd.none )

                Just ( intent, stack2 ) ->
                    applyAndSend intent model.stack { model | stack = stack2 }

        ExportGraph ->
            ( model
            , File.Download.string "graph.edited.json"
                "application/json"
                (E.encode 2 (Graph.encodeContract model.graph))
            )

        ExportLog ->
            ( model
            , File.Download.string "edit-log.json"
                "application/json"
                (E.encode 2 (Edit.encodeLog (List.reverse model.log)))
            )

        -- ── Scope ──────────────────────────────────────────────────────
        ScopeAddCommunity ->
            case currentCommunity model of
                Just cid ->
                    scopeChanged (Scope.addCommunity model.graph cid model.scope) model

                Nothing ->
                    addToast "error" "Drill into a community first" model

        ScopeAddSelection ->
            case model.nav.pos of
                Node _ nid ->
                    scopeChanged (Scope.addSelection [ nid ] model.scope) model

                _ ->
                    addToast "error" "Select a node first" model

        ScopeAddNeighborhood ->
            case model.nav.pos of
                Node _ nid ->
                    scopeChanged
                        (Scope.addNeighborhood model.graph nid model.nav.tuning.hops model.scope)
                        model

                _ ->
                    addToast "error" "Select a node first" model

        ScopeAddGroup i ->
            case List.head (List.drop i model.groups) of
                Just g ->
                    scopeChanged (Scope.addGroup model.graph g model.scope) model

                Nothing ->
                    ( model, Cmd.none )

        ScopeClear ->
            scopeChanged Scope.empty { model | scopePreview = False }

        ScopeToggleBoundary ->
            let
                m2 =
                    { model | scopeBoundary = not model.scopeBoundary }
            in
            ( m2, renderIfPreview m2 )

        ScopeTogglePreview ->
            let
                m2 =
                    { model | scopePreview = not model.scopePreview }
            in
            ( m2, renderCmd m2 )

        OpenExportScope ->
            if Scope.isEmpty model.scope then
                addToast "error" "Scope is empty — add a community, group or selection" model

            else
                armDialog (ExportScopeDialog { title = "subgraph", bake = False }) model

        ExportScopeTitle s ->
            ( { model | dialog = mapExportDialog (\f -> { f | title = s }) model.dialog }, Cmd.none )

        ExportScopeBake b ->
            ( { model | dialog = mapExportDialog (\f -> { f | bake = b }) model.dialog }, Cmd.none )

        -- ── Renderer events ────────────────────────────────────────────
        RendererEvent value ->
            handleRendererEvent value model

        DismissToast tid ->
            ( { model | toasts = List.filter (\t -> t.id /= tid) model.toasts }, Cmd.none )



-- UPDATE HELPERS


withHops : Int -> Navi.Tuning -> Navi.Tuning
withHops h tuning =
    { tuning | hops = clamp 1 6 h }


{-| The dispatcher's push/replace split (`viewer-navigation` Decision 2):
position changes push history, tuning changes replace.
-}
changeNav : Navi.ViewState -> Model -> ( Model, Cmd Msg )
changeNav newState model =
    let
        url =
            Navi.encodeHash newState

        historyCmd =
            if Navi.isPositionChange model.nav newState then
                Nav.pushUrl model.key url

            else
                Nav.replaceUrl model.key url

        m2 =
            syncEditForms { model | nav = newState, suppressUrlMsg = True }
    in
    ( m2, Cmd.batch [ historyCmd, renderCmd m2 ] )


{-| Prefill edit forms from the newly selected node.
-}
syncEditForms : Model -> Model
syncEditForms model =
    case selectedNode model of
        Just node ->
            { model | editLabel = node.label, editKind = Maybe.withDefault "" node.kind }

        Nothing ->
            model


validPos : Graph -> Position -> Bool
validPos graph pos =
    case pos of
        Overview ->
            True

        Community cid ->
            List.any (\a -> a.id == cid) graph.aggregates

        Node cid nid ->
            case Dict.get nid graph.nodes of
                Just n ->
                    n.communityId == Just cid

                Nothing ->
                    False


currentCommunity : Model -> Maybe Int
currentCommunity model =
    case model.nav.pos of
        Community cid ->
            Just cid

        Node cid _ ->
            Just cid

        Overview ->
            Nothing


selectedNode : Model -> Maybe Graph.Node
selectedNode model =
    case model.nav.pos of
        Node _ nid ->
            Dict.get nid model.graph.nodes

        _ ->
            Nothing


withSelectedNode : Model -> (Graph.Node -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
withSelectedNode model fn =
    case selectedNode model of
        Just node ->
            fn node

        Nothing ->
            ( model, Cmd.none )


{-| Both probes have returned; route to the resolved implementation. Slices
capability boots from /api/overview (never the full graph.json); otherwise fall
back to the legacy full-fetch path (`studio-data-sources` 2.2). -}
afterProbe : Model -> ( Model, Cmd Msg )
afterProbe m =
    let
        m2 =
            { m | probed = m.probed + 1 }
    in
    if m2.probed >= 2 then
        resolveFetch m2

    else
        ( m2, Cmd.none )


{-| The single fetch a resolved connection needs. -}
resolveFetch : Model -> ( Model, Cmd Msg )
resolveFetch model =
    let
        origin =
            Maybe.withDefault model.originInput model.connectingOrigin
    in
    if model.caps.slices then
        ( { model | connecting = False }
        , Api.fetchOverview origin (\r -> GotOverview ( origin, r ))
        )

    else
        ( { model | connecting = False }
        , Api.fetchGraphString origin (\r -> GotRemoteGraph ( origin, r ))
        )


{-| Shared slices-mode load path: boot from the overview (aggregates + totals +
graph hash) with zero nodes/edges until communities pull their slices on
drill-down. The overview's totals are authoritative for the stats line. -}
overviewLoaded : String -> Source.Overview -> Model -> ( Model, Cmd Msg )
overviewLoaded origin ov model =
    let
        graph =
            { nodes = Dict.empty
            , edges = Dict.empty
            , aggregates = ov.aggregates
            , communityLabels =
                Dict.fromList (List.map (\a -> ( a.id, a.label )) ov.aggregates)
            }

        navState =
            model.pendingHash
                |> Maybe.andThen Navi.decodeHash
                |> Maybe.map (Navi.sanitize (validPos graph))
                |> Maybe.withDefault Navi.initialState

        m2 =
            { model
                | source = Connected { origin = origin, caps = model.caps }
                , graph = graph
                , identity = Just ov.graphHash
                , overview = Just ov
                , nav = navState
                , pendingHash = Nothing
                , suppressUrlMsg = True
                , serverCounts = Dict.empty
                , connecting = False
            }
    in
    ( m2
    , Cmd.batch
        [ Nav.replaceUrl m2.key (Navi.encodeHash navState)
        , renderCmd m2
        ]
    )


updateCaps : (Api.Capabilities -> Api.Capabilities) -> Model -> Model
updateCaps fn model =
    let
        c =
            fn model.caps
    in
    case model.source of
        Connected k ->
            { model | caps = c, source = Connected { k | caps = c } }

        _ ->
            { model | caps = c }


mapGroupForm :
    ({ name : String, query : String, color : String } -> { name : String, query : String, color : String })
    -> Model
    -> { name : String, query : String, color : String }
mapGroupForm fn model =
    fn model.groupForm


loadFile : File -> Model -> ( Model, Cmd Msg )
loadFile file model =
    if File.size file > Source.sizeThresholdBytes then
        addToast "error"
            ("File exceeds the studio threshold ("
                ++ String.fromInt (Source.sizeThresholdBytes // (1024 * 1024))
                ++ " MB) — use connected mode against graphos serve instead"
            )
            model

    else
        ( model, Task.perform (GotFileContent (File.name file)) (File.toString file) )


{-| Shared load path (`studio-data-sources`): identity, per-graph persisted
state, deep-link restore with the stale check, replay offer.
-}
graphLoaded : Source -> String -> String -> Model -> ( Model, Cmd Msg )
graphLoaded source identity content model =
    case D.decodeString Graph.decoder content of
        Err err ->
            addToast "error" ("Not a graph.json: " ++ shortDecodeError err) model

        Ok graph ->
            let
                storedGroups =
                    Dict.get (Persist.groupsKey identity) model.stored
                        |> Maybe.andThen (Groups.decodeSet >> Result.toMaybe)
                        |> Maybe.withDefault []

                storedLog =
                    Dict.get (Persist.editlogKey identity) model.stored
                        |> Maybe.andThen (Edit.decodeLog >> Result.toMaybe)
                        |> Maybe.withDefault []

                navState =
                    model.pendingHash
                        |> Maybe.andThen Navi.decodeHash
                        |> Maybe.map (Navi.sanitize (validPos graph))
                        |> Maybe.withDefault Navi.initialState

                offerReplay =
                    case source of
                        FileMode _ ->
                            not (List.isEmpty storedLog)

                        _ ->
                            False

                m2 =
                    syncEditForms
                        { model
                            | source = source
                            , graph = graph
                            , identity = Just identity
                            , nav = navState
                            , pendingHash = Nothing
                            , suppressUrlMsg = True
                            , groups = storedGroups
                            , serverCounts = Dict.empty
                            , stack = Edit.emptyStack
                            , log = []
                            , scope = Scope.empty
                            , overview = Nothing
                            , dialog = Nothing
                        }

                ( armed, openCmd ) =
                    if offerReplay then
                        armDialog (OfferReplay storedLog) m2

                    else
                        ( m2, Cmd.none )
            in
            ( armed
            , Cmd.batch
                [ Nav.replaceUrl armed.key (Navi.encodeHash navState)
                , renderCmd armed
                , serverCountCmds armed
                , openCmd
                ]
            )



-- ── Dialog focus glue (see ports above) ────────────────────────────────────
-- The generic, unit-tested stop list: dialog container, then cancel, then
-- confirm. Every studio dialog shares it (`Studio.DialogFocus.tabStops`), so
-- the pure nextIndex/prevIndex arithmetic (mod count) stays aligned with the
-- selectors the browser glue actually moves focus to.


armDialog : Dialog -> Model -> ( Model, Cmd Msg )
armDialog newDialog model =
    ( { model | dialog = Just newDialog, dialogStops = DF.tabStops, focusIndex = 0 }
    , openDialog { stops = DF.tabStops, index = 0 }
    )



-- Move the trap to a computed stop index; the browser glue performs the focus.


focusDialogAt : Model -> Int -> ( Model, Cmd Msg )
focusDialogAt model i =
    case List.head (List.drop i model.dialogStops) of
        Just sel ->
            ( { model | focusIndex = i }, moveFocus sel )

        Nothing ->
            ( { model | focusIndex = i }, Cmd.none )



-- Clear the dialog and hand focus back to the control that opened it.


closeDialog : Model -> ( Model, Cmd Msg )
closeDialog model =
    ( { model | dialog = Nothing, focusIndex = 0 }, restoreFocus E.null )


confirmDialog : Model -> ( Model, Cmd Msg )
confirmDialog model =
    case model.dialog of
        Just (ConfirmIntent intent) ->
            let
                ( closed, cmd ) =
                    closeDialog model

                ( applied, cmd2 ) =
                    acceptIntent intent closed
            in
            ( applied, Cmd.batch [ cmd, cmd2 ] )

        Just (OfferReplay log) ->
            let
                ( closed, cmd ) =
                    closeDialog model

                m2 =
                    { closed
                        | graph = List.foldl Edit.applyIntent closed.graph log
                        , log = List.reverse log
                    }
            in
            ( m2, Cmd.batch [ cmd, renderCmd m2 ] )

        Just (ExportScopeDialog form) ->
            let
                ( closed, cmd ) =
                    closeDialog model
            in
            ( closed
            , Cmd.batch
                [ cmd
                , File.Download.string
                    (slugify form.title ++ ".json")
                    "application/json"
                    (E.encode 2
                        (Scope.encodeCanonical
                            { title = form.title, boundary = model.scopeBoundary, bakeGroups = form.bake }
                            model.graph
                            model.groups
                            model.scope
                        )
                    )
                ]
            )

        Just (GalleryDemo _) ->
            closeDialog model

        Nothing ->
            ( model, Cmd.none )


{-| Accept an edit: validate, apply optimistically, log (file mode), send the
mutation (connected mode).
-}
acceptIntent : Edit.EditIntent -> Model -> ( Model, Cmd Msg )
acceptIntent intent model =
    case Edit.validate model.graph intent of
        Err reason ->
            addToast "error" reason model

        Ok _ ->
            applyAndSend intent
                model.stack
                { model | stack = Edit.pushDone intent model.stack }


{-| Optimistic local apply + connected-mode mutation carrying the pre-action
stack snapshot for revert. File mode appends to the persisted log instead.
-}
applyAndSend : Edit.EditIntent -> Edit.UndoStack -> Model -> ( Model, Cmd Msg )
applyAndSend intent prevStack model =
    let
        m2 =
            syncEditForms { model | graph = Edit.applyIntent intent model.graph }
    in
    case m2.source of
        Connected c ->
            ( m2
            , Cmd.batch
                [ renderCmd m2
                , Api.mutate c.origin (Edit.toCypher intent) (MutateResult intent prevStack)
                ]
            )

        FileMode _ ->
            -- Offline edits (file mode): apply locally and keep the persisted
            -- edit log.
            let
                m3 =
                    { m2 | log = intent :: m2.log }
            in
            ( m3, Cmd.batch [ renderCmd m3, persistLog m3 ] )

        Disconnected _ ->
            -- Connected mode after a connection loss: apply locally and keep
            -- the edit log so a Retry can carry them across once the origin is
            -- back.
            let
                m3 =
                    { m2 | log = intent :: m2.log }
            in
            ( m3, Cmd.batch [ renderCmd m3, persistLog m3 ] )

        NoGraph ->
            ( model, Cmd.none )


persistValue : String -> E.Value -> Cmd Msg
persistValue key value =
    persist (E.object [ ( "key", E.string key ), ( "value", value ) ])


persistLog : Model -> Cmd Msg
persistLog model =
    case model.identity of
        Just identity ->
            persistValue (Persist.editlogKey identity)
                (Edit.encodeLog (List.reverse model.log))

        Nothing ->
            Cmd.none


groupsChanged : List Groups.Group -> Model -> ( Model, Cmd Msg )
groupsChanged groups model =
    let
        m2 =
            { model | groups = groups, serverCounts = Dict.empty }

        persistCmd =
            case model.identity of
                Just identity ->
                    persistValue (Persist.groupsKey identity) (Groups.encodeSet groups)

                Nothing ->
                    Cmd.none
    in
    ( m2, Cmd.batch [ persistCmd, renderCmd m2, serverCountCmds m2 ] )


{-| Server-assisted counts via /api/query when connected (until the group
evaluation endpoint of progressive-graph-interface ships).
-}
serverCountCmds : Model -> Cmd Msg
serverCountCmds model =
    case model.source of
        Connected c ->
            if c.caps.query then
                Cmd.batch
                    (List.indexedMap
                        (\i g -> Api.groupCount c.origin g.query (GotServerCount i))
                        model.groups
                    )

            else
                Cmd.none

        _ ->
            Cmd.none


scopeChanged : Scope.Scope -> Model -> ( Model, Cmd Msg )
scopeChanged scope model =
    let
        m2 =
            { model | scope = scope }
    in
    ( m2, renderIfPreview m2 )


renderIfPreview : Model -> Cmd Msg
renderIfPreview model =
    if model.scopePreview then
        renderCmd model

    else
        Cmd.none


mapExportDialog :
    ({ title : String, bake : Bool } -> { title : String, bake : Bool })
    -> Maybe Dialog
    -> Maybe Dialog
mapExportDialog fn dialog =
    case dialog of
        Just (ExportScopeDialog form) ->
            Just (ExportScopeDialog (fn form))

        other ->
            other


handleKey : { key : String, ctrl : Bool, shift : Bool, typing : Bool } -> Model -> ( Model, Cmd Msg )
handleKey k model =
    if k.key == "Escape" then
        -- Hierarchical dismissal: dialog → help overlay → navigation escape.
        if model.dialog /= Nothing then
            ( { model | dialog = Nothing }, Cmd.none )

        else if model.helpOpen then
            ( { model | helpOpen = False }, Cmd.none )

        else if model.nav.pos /= Overview then
            changeNav { pos = Navi.escape model.nav.pos, tuning = model.nav.tuning } model

        else
            ( model, Cmd.none )

    else if k.typing then
        ( model, Cmd.none )

    else if k.ctrl && k.key == "z" && not k.shift then
        update Undo model

    else if k.ctrl && (k.key == "y" || k.key == "Z" || (k.key == "z" && k.shift)) then
        update Redo model

    else if k.key == "?" then
        ( { model | helpOpen = not model.helpOpen }, Cmd.none )

    else
        ( model, Cmd.none )


handleRendererEvent : D.Value -> Model -> ( Model, Cmd Msg )
handleRendererEvent value model =
    let
        tag =
            D.decodeValue (D.field "tag" D.string) value
                |> Result.withDefault ""

        nodeId =
            D.decodeValue (D.field "id" D.string) value
                |> Result.toMaybe
    in
    case ( tag, nodeId ) of
        ( "click", Just id ) ->
            if String.startsWith "c:" id then
                case String.toInt (String.dropLeft 2 id) of
                    Just cid ->
                        changeNav { pos = Community cid, tuning = model.nav.tuning } model

                    Nothing ->
                        ( model, Cmd.none )

            else
                case Dict.get id model.graph.nodes |> Maybe.andThen .communityId of
                    Just cid ->
                        changeNav { pos = Node cid id, tuning = model.nav.tuning } model

                    Nothing ->
                        ( model, Cmd.none )

        ( "clickBackground", _ ) ->
            case model.nav.pos of
                Node cid _ ->
                    changeNav { pos = Community cid, tuning = model.nav.tuning } model

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


addToast : String -> String -> Model -> ( Model, Cmd Msg )
addToast level message model =
    addToastWithCmd level message model Cmd.none


addToastWithCmd : String -> String -> Model -> Cmd Msg -> ( Model, Cmd Msg )
addToastWithCmd level message model cmd =
    ( { model
        | toasts = List.take 4 ({ id = model.toastSeq, level = level, message = message } :: model.toasts)
        , toastSeq = model.toastSeq + 1
      }
    , cmd
    )


shortDecodeError : D.Error -> String
shortDecodeError err =
    String.left 160 (D.errorToString err)


slugify : String -> String
slugify s =
    s
        |> String.toLower
        |> String.map
            (\c ->
                if Char.isAlphaNum c then
                    c

                else
                    '-'
            )



-- LIST HELPERS


removeAt : Int -> List a -> List a
removeAt i xs =
    List.take i xs ++ List.drop (i + 1) xs


mapAt : Int -> (a -> a) -> List a -> List a
mapAt i fn =
    List.indexedMap
        (\j x ->
            if j == i then
                fn x

            else
                x
        )


moveUp : Int -> List a -> List a
moveUp i xs =
    if i <= 0 then
        xs

    else
        List.take (i - 1) xs
            ++ (List.drop (i - 1) xs |> List.take 2 |> List.reverse)
            ++ List.drop (i + 1) xs



-- RENDERER PAYLOAD


{-| Everything the canvas shows, derived from the Model: LOD phase from the
position, facet filtering, group colors (first-match-wins), isolation,
scope-preview dimming, theme canvas tokens.
-}
renderCmd : Model -> Cmd Msg
renderCmd model =
    let
        t =
            Tokens.tokens model.theme
    in
    if Dict.isEmpty model.graph.nodes && List.isEmpty model.graph.aggregates then
        Cmd.none

    else
        toRenderer
            (E.object
                [ ( "tag", E.string "render" )
                , ( "canvas"
                  , E.object
                        [ ( "bg", E.string t.canvasBg )
                        , ( "edge", E.string t.canvasEdge )
                        , ( "label", E.string t.canvasLabel )
                        ]
                  )
                , ( "fit", E.bool True )
                , ( "selected"
                  , case model.nav.pos of
                        Node _ nid ->
                            E.string nid

                        _ ->
                            E.null
                  )
                , ( "nodes", E.list identity (renderNodes model) )
                , ( "edges", E.list identity (renderEdges model) )
                ]
            )


renderNodes : Model -> List E.Value
renderNodes model =
    case model.nav.pos of
        Overview ->
            List.map (renderAggregateDot model) model.graph.aggregates

        Community cid ->
            renderMembers model cid

        Node cid _ ->
            renderMembers model cid


renderAggregateDot : Model -> Graph.Aggregate -> E.Value
renderAggregateDot model agg =
    E.object
        [ ( "id", E.string ("c:" ++ String.fromInt agg.id) )
        , ( "label", E.string (agg.label ++ " (" ++ String.fromInt agg.memberCount ++ ")") )
        , ( "color", E.string (Tokens.harmonize model.theme agg.color) )
        , ( "value", E.int agg.memberCount )
        , ( "shape", E.string "dot" )
        ]


renderMembers : Model -> Int -> List E.Value
renderMembers model cid =
    let
        t =
            Tokens.tokens model.theme

        members =
            Graph.communityMembers model.graph cid

        facets =
            model.nav.tuning.facets

        facetKeep node =
            List.isEmpty facets
                || List.member (Graph.fileTypeToString node.fileType) facets

        groupIdx =
            Groups.memberships model.groups model.graph

        isolationMap =
            Groups.visibleUnderIsolation model.groups model.graph

        groupOf node =
            Dict.get node.id groupIdx
                |> Maybe.andThen (\i -> List.head (List.drop i model.groups))

        visible node =
            (case isolationMap of
                Just vis ->
                    Dict.get node.id vis == Just True

                Nothing ->
                    True
            )
                && (case groupOf node of
                        Just g ->
                            not g.hidden

                        Nothing ->
                            True
                   )

        communityColor =
            model.graph.aggregates
                |> List.filter (\a -> a.id == cid)
                |> List.head
                |> Maybe.map (.color >> Tokens.harmonize model.theme)
                |> Maybe.withDefault t.canvasLabel

        scopeAll =
            if model.scopePreview then
                Just
                    (Set.union model.scope
                        (Scope.scopeEdges model.graph model.scopeBoundary model.scope).boundaryNodes
                    )

            else
                Nothing

        colorOf node =
            let
                base =
                    case groupOf node of
                        Just g ->
                            g.color

                        Nothing ->
                            communityColor
            in
            case scopeAll of
                Just inScope ->
                    if Set.member node.id inScope then
                        base

                    else
                        t.canvasDim

                Nothing ->
                    base
    in
    members
        |> List.filter facetKeep
        |> List.filter visible
        |> List.map
            (\node ->
                E.object
                    [ ( "id", E.string node.id )
                    , ( "label", E.string node.label )
                    , ( "color", E.string (colorOf node) )
                    , ( "shape", E.string "dot" )
                    , ( "value", E.int (Maybe.withDefault 1 node.degree) )
                    ]
            )


renderEdges : Model -> List E.Value
renderEdges model =
    case currentCommunity model of
        Nothing ->
            []

        Just cid ->
            let
                memberIds =
                    Graph.communityMembers model.graph cid
                        |> List.map .id
                        |> Set.fromList
            in
            Dict.values model.graph.edges
                |> List.filter (\e -> Set.member e.source memberIds && Set.member e.target memberIds)
                |> List.map
                    (\e ->
                        E.object
                            [ ( "id", E.string e.id )
                            , ( "from", E.string e.source )
                            , ( "to", E.string e.target )
                            ]
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fromRenderer RendererEvent
        , Browser.Events.onKeyDown keyDecoder
        ]


keyDecoder : D.Decoder Msg
keyDecoder =
    D.map4
        (\key ctrl shift tag ->
            KeyPressed
                { key = key
                , ctrl = ctrl
                , shift = shift
                , typing = List.member tag [ "INPUT", "TEXTAREA", "SELECT" ]
                }
        )
        (D.field "key" D.string)
        (D.oneOf [ D.field "ctrlKey" D.bool, D.succeed False ])
        (D.oneOf [ D.field "shiftKey" D.bool, D.succeed False ])
        (D.oneOf [ D.at [ "target", "tagName" ] D.string, D.succeed "" ])



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Graphos Studio"
    , body =
        [ Html.node "style" [] [ text (Tokens.stylesheet model.theme) ]
        , div [ A.class "studio-shell" ]
            ([ viewTopbar model
             , div [ A.class "studio-main" ]
                (if model.galleryOpen then
                    [ viewGallery model ]

                 else
                    case model.source of
                        NoGraph ->
                            [ viewWelcome model ]

                        _ ->
                            [ viewSidebar model
                            , div [ A.class "studio-canvas" ] [ div [ A.id "graph-canvas" ] [] ]
                            ]
                                ++ (case selectedNode model of
                                        Just node ->
                                            [ viewDetail model node ]

                                        Nothing ->
                                            []
                                   )
                )
             , viewToasts model
             ]
                ++ viewDialog model
                ++ viewHelp model
            )
        ]
    }


viewTopbar : Model -> Html Msg
viewTopbar model =
    div [ A.class "studio-topbar" ]
        [ h1 [ A.style "font-size" "inherit" ] [ text "Graphos Studio" ]
        , viewCrumbs model
        , div [ A.style "flex" "1" ] []
        , UI.btn { label = "↶ Undo", enabled = not (List.isEmpty model.stack.done), onPress = Undo }
        , UI.btn { label = "↷ Redo", enabled = not (List.isEmpty model.stack.undone), onPress = Redo }
        , UI.btn
            { label =
                case model.theme of
                    Light ->
                        "◑ Dark"

                    Dark ->
                        "◐ Light"
            , enabled = True
            , onPress = ToggleTheme
            }
        , UI.btn
            { label = "▦ Gallery", enabled = True, onPress = ToggleGallery }
        , UI.btn { label = "?", enabled = True, onPress = ToggleHelp }
        ]


viewCrumbs : Model -> Html Msg
viewCrumbs model =
    let
        segments =
            Navi.breadcrumb model.nav.pos

        lastIndex =
            List.length segments - 1

        segLabel pos =
            case pos of
                Overview ->
                    "Overview"

                Community cid ->
                    Graph.communityLabel model.graph cid

                Node _ nid ->
                    Dict.get nid model.graph.nodes
                        |> Maybe.map .label
                        |> Maybe.withDefault nid

        renderSeg i pos =
            let
                lbl =
                    segLabel pos
            in
            if i == lastIndex then
                [ span [ A.class "crumb-current", A.title lbl ] [ text lbl ] ]

            else
                [ Html.button [ A.class "crumb-link", Ev.onClick (NavigateTo pos) ] [ text lbl ]
                , span [ A.class "crumb-sep" ] [ text "▸" ]
                ]
    in
    div [ A.class "crumbs" ] (List.concat (List.indexedMap renderSeg segments))


viewWelcome : Model -> Html Msg
viewWelcome model =
    div
        [ A.style "flex" "1"
        , A.style "display" "flex"
        , A.style "align-items" "center"
        , A.style "justify-content" "center"
        ]
        [ div [ A.style "width" "480px" ]
            [ UI.panel "Open a graph"
                [ div
                    [ A.class
                        (if model.dropActive then
                            "drop-zone drop-zone-active"

                         else
                            "drop-zone"
                        )
                    , Ev.preventDefaultOn "dragover" (D.succeed ( DragEnter, True ))
                    , Ev.on "dragleave" (D.succeed DragLeave)
                    , Ev.preventDefaultOn "drop" (D.map (\f -> ( GotDropFile f, True )) dropFileDecoder)
                    ]
                    [ text "Drop a graph.json here" ]
                , div [ A.style "text-align" "center", A.style "margin" "8px 0" ]
                    [ span [ A.class "muted" ] [ text "or" ] ]
                , UI.btnPrimary { label = "Choose a file…", enabled = True, onPress = PickFile }
                ]
            , UI.panel "Connect to graphos serve"
                [ UI.field "Origin"
                    (UI.textInput { value = model.originInput, placeholder = "http://localhost:8080", onInput = OriginInput })
                , UI.btnPrimary
                    { label =
                        if model.connecting then
                            "Connecting…"

                        else
                            "Connect"
                    , enabled = not model.connecting
                    , onPress = Connect
                    }
                ]
            ]
        ]


dropFileDecoder : D.Decoder File
dropFileDecoder =
    D.at [ "dataTransfer", "files" ] (D.index 0 File.decoder)


viewSidebar : Model -> Html Msg
viewSidebar model =
    div [ A.class "studio-sidebar" ]
        ([ viewSourcePanel model
         , viewTuningPanel model
         , viewGroupsPanel model
         , viewScopePanel model
         ]
            ++ (case model.source of
                    FileMode _ ->
                        [ viewFileEditPanel model ]

                    _ ->
                        []
               )
        )


viewSourcePanel : Model -> Html Msg
viewSourcePanel model =
    UI.panel "Source"
        (case model.source of
            NoGraph ->
                [ UI.emptyState "No graph loaded" ]

            FileMode name ->
                [ div [ A.class "spread" ]
                    [ span [] [ text name ]
                    , UI.badge "" "file mode"
                    ]
                , viewGraphStats model
                , UI.btn { label = "Close graph", enabled = True, onPress = Disconnect }
                ]

            Connected c ->
                [ div [ A.class "spread" ]
                    [ span [] [ text c.origin ]
                    , UI.badge "accent" "connected"
                    ]
                , div [ A.class "row" ]
                    [ UI.badge ""
                        (if c.caps.slices then
                            "slices mode"

                         else
                            "legacy (full fetch)"
                        )
                    , UI.badge ""
                        (if c.caps.query then
                            "query ✓"

                         else
                            "query ✗"
                        )
                    , UI.badge ""
                        (if c.caps.slices then
                            "slices ✓"

                         else
                            "slices ✗ (full fetch)"
                        )
                    ]
                , viewGraphStats model
                , UI.btn { label = "Disconnect", enabled = True, onPress = ConnectionLost }
                ]

            Disconnected k ->
                [ div [ A.class "spread" ]
                    [ span [] [ text k.origin ]
                    , UI.badge "" "disconnected"
                    ]
                , div [ A.class "row" ]
                    [ UI.btn { label = "Retry", enabled = True, onPress = Retry }
                    , UI.btn { label = "Clear everything", enabled = True, onPress = Discard }
                    ]
                , div [ A.class "muted" ]
                    [ text "Working offline — your edits are kept locally and replayed on retry." ]
                ]
        )


viewGraphStats : Model -> Html Msg
viewGraphStats model =
    let
        ( n, e, c ) =
            case model.overview of
                Just ov ->
                    -- Overview totals are authoritative in slices mode (the
                    -- graph is loaded lazily via slices, so local node/edge
                    -- counts start empty).
                    ( ov.nodeCount, ov.edgeCount, ov.communityCount )

                Nothing ->
                    ( Graph.nodeCount model.graph
                    , Graph.edgeCount model.graph
                    , List.length model.graph.aggregates
                    )

        identity =
            model.identity
                |> Maybe.map (\h -> "graph " ++ String.left 16 h)
                |> Maybe.withDefault ""

        statsText =
            String.fromInt n
                ++ " nodes · "
                ++ String.fromInt e
                ++ " edges · "
                ++ String.fromInt c
                ++ " communities"
    in
    div [ A.class "muted" ]
    ( List.concat
        [ [ text statsText ]
        , if identity == "" then
            []

          else
            [ div [ A.class "hash" ] [ text identity ] ]
        ]
    )


viewTuningPanel : Model -> Html Msg
viewTuningPanel model =
    let
        facet ft =
            let
                name =
                    Graph.fileTypeToString ft

                active =
                    List.member name model.nav.tuning.facets
            in
            Html.label [ A.class "list-item row" ]
                [ input [ A.type_ "checkbox", A.checked active, Ev.onCheck (\_ -> ToggleFacet name) ] []
                , text name
                ]
    in
    UI.panel "View"
        [ UI.field ("Neighborhood hops: " ++ String.fromInt model.nav.tuning.hops)
            (UI.slider { min = 1, max = 6, value = model.nav.tuning.hops, onChange = SetHops })
        , UI.field "File-type facets (empty = all)"
            (div []
                (List.map facet
                    [ Graph.CodeFile
                    , Graph.DocFile
                    , Graph.PaperFile
                    , Graph.ImageFile
                    , Graph.VideoFile
                    , Graph.AudioFile
                    , Graph.OfficeFile
                    ]
                )
            )
        ]


viewGroupsPanel : Model -> Html Msg
viewGroupsPanel model =
    let
        localCounts =
            Groups.memberships model.groups model.graph
                |> Dict.foldl
                    (\_ i acc -> Dict.update i (\c -> Just (Maybe.withDefault 0 c + 1)) acc)
                    Dict.empty

        evalBadge =
            case model.source of
                Connected c ->
                    if c.caps.query then
                        UI.badge "accent" "local + server counts"

                    else
                        UI.badge "" "local eval"

                _ ->
                    UI.badge "" "local eval"

        viewGroup i g =
            div [ A.class "panel", A.style "margin-bottom" "8px" ]
                [ div [ A.class "spread" ]
                    [ div [ A.class "row" ]
                        [ span [ A.class "group-color-dot", A.style "background" g.color ] []
                        , span [] [ text g.name ]
                        ]
                    , span [ A.class "muted" ]
                        [ text
                            (String.fromInt (Maybe.withDefault 0 (Dict.get i localCounts))
                                ++ (case Dict.get i model.serverCounts of
                                        Just n ->
                                            " · srv " ++ String.fromInt n

                                        Nothing ->
                                            ""
                                   )
                            )
                        ]
                    ]
                , div [ A.class "muted" ] [ text g.query ]
                , div [ A.class "row", A.style "margin-top" "4px" ]
                    [ UI.btn
                        { label =
                            if g.hidden then
                                "Show"

                            else
                                "Hide"
                        , enabled = True
                        , onPress = ToggleGroupHidden i
                        }
                    , UI.btn
                        { label =
                            if g.isolated then
                                "Un-isolate"

                            else
                                "Isolate"
                        , enabled = True
                        , onPress = ToggleGroupIsolated i
                        }
                    , UI.btn { label = "↑", enabled = i > 0, onPress = MoveGroupUp i }
                    , UI.btn { label = "+scope", enabled = True, onPress = ScopeAddGroup i }
                    , UI.btnDanger { label = "✕", enabled = True, onPress = DeleteGroup i }
                    ]
                ]
    in
    UI.panel "Groups"
        ([ div [ A.class "spread" ]
            [ evalBadge
            , div [ A.class "row" ]
                [ UI.btn { label = "Import", enabled = True, onPress = ImportGroups }
                , UI.btn { label = "Export", enabled = not (List.isEmpty model.groups), onPress = ExportGroups }
                ]
            ]
         ]
            ++ List.indexedMap viewGroup model.groups
            ++ [ UI.field "Name" (UI.textInput { value = model.groupForm.name, placeholder = "auth", onInput = GroupFormName })
               , UI.field "Query" (UI.textInput { value = model.groupForm.query, placeholder = "auth token", onInput = GroupFormQuery })
               , UI.field "Color"
                    (input
                        [ A.type_ "color"
                        , A.value model.groupForm.color
                        , Ev.onInput GroupFormColor
                        ]
                        []
                    )
               , UI.btnPrimary { label = "Add group", enabled = True, onPress = AddGroup }
               ]
        )


viewScopePanel : Model -> Html Msg
viewScopePanel model =
    let
        c =
            Scope.counts model.graph model.scopeBoundary model.scope
    in
    UI.panel "Extract subgraph"
        [ div [ A.class "muted" ]
            [ text (String.fromInt c.nodes ++ " nodes · " ++ String.fromInt c.edges ++ " edges in scope") ]
        , div [ A.class "row", A.style "flex-wrap" "wrap", A.style "margin-top" "4px" ]
            [ UI.btn { label = "+ community", enabled = currentCommunity model /= Nothing, onPress = ScopeAddCommunity }
            , UI.btn { label = "+ selection", enabled = selectedNode model /= Nothing, onPress = ScopeAddSelection }
            , UI.btn { label = "+ neighborhood", enabled = selectedNode model /= Nothing, onPress = ScopeAddNeighborhood }
            ]
        , Html.label [ A.class "list-item row" ]
            [ input [ A.type_ "checkbox", A.checked model.scopeBoundary, Ev.onCheck (\_ -> ScopeToggleBoundary) ] []
            , text "include boundary edges (1 hop out)"
            ]
        , div [ A.class "row", A.style "margin-top" "4px" ]
            [ UI.btn
                { label =
                    if model.scopePreview then
                        "Exit preview"

                    else
                        "Preview"
                , enabled = not (Scope.isEmpty model.scope)
                , onPress = ScopeTogglePreview
                }
            , UI.btn { label = "Clear", enabled = not (Scope.isEmpty model.scope), onPress = ScopeClear }
            , UI.btnPrimary { label = "Export…", enabled = not (Scope.isEmpty model.scope), onPress = OpenExportScope }
            ]
        ]


viewFileEditPanel : Model -> Html Msg
viewFileEditPanel model =
    UI.panel "Edited graph"
        [ div [ A.class "muted" ]
            [ text (String.fromInt (List.length model.log) ++ " edits in this session's log") ]
        , div [ A.class "row", A.style "margin-top" "4px" ]
            [ UI.btn { label = "Export graph.json", enabled = not (List.isEmpty model.log), onPress = ExportGraph }
            , UI.btn { label = "Export changelog", enabled = not (List.isEmpty model.log), onPress = ExportLog }
            ]
        ]


viewDetail : Model -> Graph.Node -> Html Msg
viewDetail model node =
    let
        edges =
            Graph.incidentEdges model.graph node.id

        neighborChip e =
            let
                ( other, arrow ) =
                    if e.source == node.id then
                        ( e.target, "→ " )

                    else
                        ( e.source, "← " )

                lbl =
                    Dict.get other model.graph.nodes
                        |> Maybe.map .label
                        |> Maybe.withDefault other
            in
            div [ A.class "list-item spread" ]
                [ span
                    [ Ev.onClick
                        (case Dict.get other model.graph.nodes |> Maybe.andThen .communityId of
                            Just cid ->
                                NavigateTo (Node cid other)

                            Nothing ->
                                NoOp
                        )
                    ]
                    [ text (arrow ++ e.relation ++ " " ++ lbl) ]
                , UI.btnDanger { label = "✕", enabled = True, onPress = RequestDeleteEdge e.id }
                ]

        info label_ value_ =
            div [ A.class "spread" ]
                [ span [ A.class "muted" ] [ text label_ ], span [] [ text value_ ] ]
    in
    div [ A.class "studio-detail" ]
        [ UI.panel node.label
            [ info "id" node.id
            , info "kind" (Maybe.withDefault "—" node.kind)
            , info "type" (Graph.fileTypeToString node.fileType)
            , info "source"
                (node.sourceFile
                    ++ (case node.lineStart of
                            Just l ->
                                ":" ++ String.fromInt l

                            Nothing ->
                                ""
                       )
                )
            , info "community"
                (case node.communityId of
                    Just cid ->
                        Graph.communityLabel model.graph cid

                    Nothing ->
                        "—"
                )
            , info "degree" (String.fromInt (List.length edges))
            , info "bridge"
                (if node.isBridge == Just True then
                    "yes"

                 else
                    "no"
                )
            ]
        , UI.panel ("Neighbours (" ++ String.fromInt (List.length edges) ++ ")")
            (List.map neighborChip (List.take 40 edges)
                ++ (if List.length edges > 40 then
                        [ div [ A.class "muted" ]
                            [ text ("… and " ++ String.fromInt (List.length edges - 40) ++ " more") ]
                        ]

                    else
                        []
                   )
            )
        , viewEditPanel model node
        ]


viewEditPanel : Model -> Graph.Node -> Html Msg
viewEditPanel model node =
    UI.panel "Edit"
        [ UI.field "Label" (UI.textInput { value = model.editLabel, placeholder = node.label, onInput = EditLabelInput })
        , UI.btn { label = "Rename", enabled = model.editLabel /= node.label, onPress = SubmitRelabel }
        , UI.field "Kind" (UI.textInput { value = model.editKind, placeholder = "Function", onInput = EditKindInput })
        , UI.btn { label = "Set kind", enabled = model.editKind /= Maybe.withDefault "" node.kind, onPress = SubmitRetype }
        , UI.field "New edge to (node id)"
            (UI.textInput { value = model.edgeTarget, placeholder = "target node id", onInput = EdgeTargetInput })
        , UI.field "Relation"
            (UI.selectInput
                { value = model.edgeRelation
                , options =
                    List.map (\r -> ( r, r ))
                        [ "calls", "imports", "extends", "implements", "references", "contains", "depends_on", "inferred" ]
                , onSelect = EdgeRelationInput
                }
            )
        , UI.btn { label = "Add edge", enabled = model.edgeTarget /= "", onPress = SubmitCreateEdge }
        , Html.hr [] []
        , UI.field "New node id" (UI.textInput { value = model.newNodeId, placeholder = "concept_auth_flow", onInput = NewNodeIdInput })
        , UI.field "New node label" (UI.textInput { value = model.newNodeLabel, placeholder = "Auth flow", onInput = NewNodeLabelInput })
        , UI.btn { label = "Create node", enabled = model.newNodeId /= "" && model.newNodeLabel /= "", onPress = SubmitCreateNode }
        , Html.hr [] []
        , UI.btnDanger { label = "Delete this node…", enabled = True, onPress = RequestDeleteNode }
        ]


viewToasts : Model -> Html Msg
viewToasts model =
    div [ A.class "toasts" ]
        (List.map
            (\t ->
                div [ Ev.onClick (DismissToast t.id) ] [ UI.toast { level = t.level, message = t.message } ]
            )
            model.toasts
        )


viewDialog : Model -> List (Html Msg)
viewDialog model =
    case model.dialog of
        Nothing ->
            []

        Just (ConfirmIntent intent) ->
            [ UI.dialog
                { title = "Confirm edit"
                , body = [ text (Edit.describe intent) ]
                , confirmLabel = "Apply"
                , onConfirm = ConfirmDialog
                , onCancel = CancelDialog
                , onTab = DialogTab
                , onShiftTab = DialogShiftTab
                , destructive = True
                }
            ]

        Just (OfferReplay log) ->
            [ UI.dialog
                { title = "Stored edits found"
                , body =
                    [ text
                        ("This graph has "
                            ++ String.fromInt (List.length log)
                            ++ " stored edits from a previous session. Replay them?"
                        )
                    ]
                , confirmLabel = "Replay"
                , onConfirm = ConfirmDialog
                , onCancel = CancelDialog
                , onTab = DialogTab
                , onShiftTab = DialogShiftTab
                , destructive = False
                }
            ]

        Just (ExportScopeDialog form) ->
            [ UI.dialog
                { title = "Export subgraph"
                , body =
                    [ UI.field "Title"
                        (UI.textInput { value = form.title, placeholder = "auth-core", onInput = ExportScopeTitle })
                    , Html.label [ A.class "list-item row" ]
                        [ input [ A.type_ "checkbox", A.checked form.bake, Ev.onCheck ExportScopeBake ] []
                        , text "bake group colors into nodes"
                        ]
                    ]
                , confirmLabel = "Download"
                , onConfirm = ConfirmDialog
                , onCancel = CancelDialog
                , onTab = DialogTab
                , onShiftTab = DialogShiftTab
                , destructive = False
                }
            ]

        Just (GalleryDemo msg) ->
            [ UI.dialog
                { title = "Keyboard-complete dialog"
                , body = [ text msg.message ]
                , confirmLabel = "Got it"
                , onConfirm = GalleryConfirmDialog
                , onCancel = CancelDialog
                , onTab = DialogTab
                , onShiftTab = DialogShiftTab
                , destructive = False
                }
            ]


viewGallery : Model -> Html Msg
viewGallery model =
    div [ A.class "gallery" ]
        [ div [ A.class "gallery-head" ]
            [ h2 [] [ text "Component gallery" ]
            , span [ A.class "muted" ]
                [ text " Every control below is theme-correct. Toggle "
                , Html.button [ A.class "btn", Ev.onClick ToggleTheme ] [ text "◑◐" ]
                , text " above — each re-renders with the active Tokens."
                ]
            ]
        , div [ A.class "gallery-grid" ]
            [ UI.panel "Buttons"
                [ div [ A.class "row" ]
                    [ UI.btn { label = "Default", enabled = True, onPress = GalleryNoop }
                    , UI.btnPrimary { label = "Primary", enabled = True, onPress = GalleryNoop }
                    , UI.btnDanger { label = "Danger", enabled = True, onPress = GalleryNoop }
                    , UI.btn { label = "Disabled", enabled = False, onPress = GalleryNoop }
                    ]
                , UI.btn
                    { label = "Open a keyboard-complete dialog", enabled = True, onPress = OpenGalleryDemo }
                ]
            , UI.panel "Text input"
                [ UI.field "Editable text"
                    (UI.textInput
                        { value = model.galleryTitle
                        , placeholder = "Type here..."
                        , onInput = GalleryInput
                        }
                    )
                ]
            , UI.panel "Select"
                [ UI.field "Kind"
                    (UI.selectInput
                        { value = model.gallerySelect
                        , options =
                            [ ( "doc", "Documents" )
                            , ( "code", "Code" )
                            , ( "meta", "Metadata" )
                            ]
                        , onSelect = GallerySelect
                        }
                    )
                ]
            , UI.panel "Slider"
                [ UI.field "Intensity"
                    (UI.slider
                        { min = 0
                        , max = 100
                        , value = model.gallerySlider
                        , onChange = GallerySlider
                        }
                    )
                ]
            , UI.panel "Badges + empty state"
                [ div [ A.class "row" ]
                    [ UI.badge "default" "default"
                    , UI.badge "accent" "accent"
                    , UI.badge "danger" "danger"
                    ]
                , UI.emptyState "No entries match this group."
                ]
            , UI.panel "Toast + panel"
                [ UI.toast { level = "success", message = "Re-styling with the active theme." }
                , UI.panel "Container panel"
                    [ p [ A.class "muted" ] [ text "Panels and their contents inherit theme tokens." ]
                    , UI.btn { label = "Do something", enabled = True, onPress = GalleryNoop }
                    ]
                ]
            , UI.panel "Dialog (openable)"
                [ UI.btn
                    { label = "Open a modal", enabled = True, onPress = OpenGalleryDemo }
                ]
            ]
        ]


viewHelp : Model -> List (Html Msg)
viewHelp model =
    if not model.helpOpen then
        []

    else
        [ div [ A.class "dialog-backdrop", Ev.onClick ToggleHelp ]
            [ div [ A.class "dialog" ]
                [ div [ A.class "dialog-title" ] [ text "Keyboard shortcuts" ]
                , shortcutRow "Esc" "close dialog / help → deselect node → up to overview"
                , shortcutRow "Ctrl+Z" "undo"
                , shortcutRow "Ctrl+Shift+Z / Ctrl+Y" "redo"
                , shortcutRow "?" "toggle this overlay"
                ]
            ]
        ]


shortcutRow : String -> String -> Html Msg
shortcutRow keys desc =
    div [ A.class "spread", A.style "margin-bottom" "8px" ]
        [ span [ A.class "kbd" ] [ text keys ], span [ A.class "muted" ] [ text desc ] ]



-- MAIN


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
