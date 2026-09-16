module Studio.Persist exposing
    ( filterFor
    , groupsKey
    , key
    , editlogKey
    , tuningKey
    )

{-| Persistence-key factory and per-graph isolation (`studio-data-sources` graph
identity). Every persisted per-graph artifact is keyed by

    "graphos-studio:" ++ artifact ++ ":" ++ identity

so a change of graph identity never silently mixes data: resident graph data is
dropped and re-fetched, while persisted per-graph state for the previous
identity stays stored under its own key (corpus A/B isolation at the logic
level). `identity` is the server graph hash (connected mode) or the file content
fingerprint (file mode).

-}

import Dict exposing (Dict)
import Json.Decode as D



-- KEY FACTORY


{-| The one key shape. Centralized so file mode and connected mode agree on the
naming and identity-change logic stays in one place. -}
key : String -> String -> String
key artifact identity =
    "graphos-studio:" ++ artifact ++ ":" ++ identity



-- PER-ARTIFACT KEYS


groupsKey : String -> String
groupsKey identity =
    key "groups" identity


editlogKey : String -> String
editlogKey identity =
    key "editlog" identity


tuningKey : String -> String
tuningKey identity =
    key "tuning" identity



-- A/B ISOLATION


{-| Keep only the keys that belong to `identity`, discarding every other
identity's entries from what is read (their bytes stay stored, just not read).
This is the logic-level corpus A/B guard: reading corpus B never surfaces
corpus A's groups, while A's data remains under A's own key. -}
filterFor : String -> Dict String D.Value -> Dict String D.Value
filterFor identity stored =
    let
        prefix =
            "graphos-studio:"

        isMine k =
            if String.startsWith prefix k then
                let
                    rest =
                        String.dropLeft (String.length prefix) k
                in
                rest == ("groups:" ++ identity)
                    || rest == ("editlog:" ++ identity)
                    || rest == ("tuning:" ++ identity)

            else
                False
    in
    Dict.filter (\k _ -> isMine k) stored
