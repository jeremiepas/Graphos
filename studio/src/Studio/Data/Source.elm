module Studio.Data.Source exposing
    ( ConnMode(..)
    , ConnStatus
    , SourceKind(..)
    , NodeSlice
    , Overview
    , NeighborhoodSlice
    , sizeThresholdBytes
    , exceedsSizeLimit
    , resolveKind
    , resolveMode
    , slicesAvailable
    , slicesReason
    , connStatusLabel
    , overviewDecoder
    , communitySliceDecoder
    , neighborhoodSliceDecoder
    )

{-| One data-source interface (`studio-data-sources`). Three implementations
are resolved from capability detection at connect time:

  - `FileSource` — offline, no network. Aggregates are synthesized client-side
    from node community fields; the size guard refuses oversized files before
    reading. Used when no origin is connected.
  - `SlicesMode` — the server ships the slice API
    (`progressive-graph-interface`). A fresh load boots from `/api/overview`
    (aggregates + totals + graph hash) and never requests the full `graph.json`;
    communities and neighborhoods are pulled as cursor-paged slices on drill-down.
  - `LegacyMode` — the server has no slice API. The studio falls back to a
    single full `/graph.json` fetch; slice-dependent affordances are labeled
    with their unavailability reason.

The pure resolution, status labels, size guard and wire-format decoders live
here (all unit-testable). The HTTP effects that service them live in
`Studio.Api`; `Main.elm` routes through this interface by resolved mode.

-}

import Json.Decode as D
import Studio.Api as Api
import Studio.Data.Graph as Graph



-- CONNECTION STATUS


type ConnStatus
    = Live
    | Disconnected


connStatusLabel : ConnStatus -> String
connStatusLabel status =
    case status of
        Live ->
            "live"

        Disconnected ->
            "disconnected"



-- RESOLVED MODE / KIND


type ConnMode
    = SlicesMode
    | LegacyMode


type SourceKind
    = FileSource
    | SlicesSource
    | LegacySource


{-| Which implementation capability detection selects. Slices are only usable
when the server answers the overview/slice probe; otherwise fall back to the
legacy full-fetch path (never two code paths in every feature). -}
resolveMode : Api.Capabilities -> ConnMode
resolveMode caps =
    if caps.slices then
        SlicesMode

    else
        LegacyMode


{-| The source kind for a resolved mode (file when nothing is connected). -}
resolveKind : Api.Capabilities -> Maybe String -> SourceKind
resolveKind caps origin =
    case origin of
        Nothing ->
            FileSource

        Just "" ->
            FileSource

        _ ->
            if caps.slices then
                SlicesSource

            else
                LegacySource


{-| Slice-dependent affordances (overview-only boot, drill-down slices) are
available only in the slices implementation. -}
slicesAvailable : ConnMode -> Bool
slicesAvailable mode =
    case mode of
        SlicesMode ->
            True

        LegacyMode ->
            False


{-| Reason string shown on a slice-dependent badge/toast when unavailable
(spec: name the reason). -}
slicesReason : ConnMode -> String
slicesReason mode =
    case mode of
        SlicesMode ->
            "slices available"

        LegacyMode ->
            "server has no slice API — drill-down needs the full graph"



-- FILE-MODE SIZE GUARD


{-| File-mode size guard (`studio-data-sources`). Refuse files above this before
reading, so an oversized file keeps the tab responsive and recommends connected
mode. Measured on the reference machine (see `studio/README.md`); set from data,
not picked arbitrarily. -}
sizeThresholdBytes : Int
sizeThresholdBytes =
    100 * 1024 * 1024



-- FILE-MODE SIZE GUARD


{-| Refuse files above `limit` before reading, so an oversized file keeps the
tab responsive and recommends connected mode. Pure: shared by the file picker
and drag-drop in `Main.elm`. -}
exceedsSizeLimit : Int -> Int -> Bool
exceedsSizeLimit limit size =
    size > limit



-- WIRE FORMAT: OVERVIEW (`GET /api/overview`)


{-| Aggregates + graph totals + content hash. Zero nodes or edges (per the slice
API spec): this is the only data a fresh remote-mode load requires. -}
type alias Overview =
    { aggregates : List Graph.Aggregate
    , nodeCount : Int
    , edgeCount : Int
    , communityCount : Int
    , graphHash : String
    }


{-| Decode `/api/overview`. Accepts `graph_hash` or the older `hash` name. -}
overviewDecoder : D.Decoder Overview
overviewDecoder =
    D.map5 Overview
        (D.field "community_aggregates" (D.list Graph.aggregateDecoder))
        (D.oneOf [ D.field "node_count" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "edge_count" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "community_count" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "graph_hash" D.string, D.field "hash" D.string ])



-- WIRE FORMAT: COMMUNITY SLICE (`GET /api/slice/community`)


{-| One page of a community's member nodes and internal edges, hubs-first, with
totals and the next cursor (absent on the last page). -}
type alias NodeSlice =
    { communityId : Int
    , nodes : List Graph.Node
    , edges : List Graph.Edge
    , totalMembers : Int
    , totalInternalEdges : Int
    , nextCursor : Maybe String
    }


communitySliceDecoder : D.Decoder NodeSlice
communitySliceDecoder =
    D.map6 NodeSlice
        (D.field "community_id" D.int)
        (D.field "nodes" (D.list Graph.nodeDecoder))
        (D.field "edges" (D.list Graph.edgeDecoder))
        (D.oneOf [ D.field "total_members" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "total_internal_edges" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "next_cursor" (D.nullable D.string), D.succeed Nothing ])



-- WIRE FORMAT: NEIGHBORHOOD SLICE (`GET /api/slice/neighborhood`)


{-| BFS neighborhood page of one node, with totals and a truncation flag. -}
type alias NeighborhoodSlice =
    { nodes : List Graph.Node
    , edges : List Graph.Edge
    , totalSize : Int
    , truncated : Bool
    }


neighborhoodSliceDecoder : D.Decoder NeighborhoodSlice
neighborhoodSliceDecoder =
    D.map4 NeighborhoodSlice
        (D.field "nodes" (D.list Graph.nodeDecoder))
        (D.field "edges" (D.list Graph.edgeDecoder))
        (D.oneOf [ D.field "total_size" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "truncated" D.bool, D.succeed False ])
