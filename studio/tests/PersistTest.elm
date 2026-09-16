module PersistTest exposing (suite)

{-| `Studio.Persist`: the identity-keyed persistence-key factory and corpus
A/B isolation logic (`studio-data-sources` graph identity). Logic-level only —
no browser required. -}

import Dict exposing (Dict)
import Json.Decode as D
import Expect
import List
import Studio.Persist as Persist
import Test exposing (Test, describe, test)


corpusA : String
corpusA =
    "fp-corpus-a"


corpusB : String
corpusB =
    "fp-corpus-b"


{-| A stored bag holding both corpora's per-graph state under their own keys,
plus an unrelated artifact (theme). Decoded from a JSON object the same way the
port layer restores persisted values on init. -}
both : Dict String D.Value
both =
    let
        json =
            """
{
  "graphos-studio:groups:fp-corpus-a": "a-groups",
  "graphos-studio:editlog:fp-corpus-a": "a-edit",
  "graphos-studio:tuning:fp-corpus-a": "a-tuning",
  "graphos-studio:groups:fp-corpus-b": "b-groups",
  "graphos-studio:editlog:fp-corpus-b": "b-edit",
  "graphos-studio:tuning:fp-corpus-b": "b-tuning",
  "graphos-studio:theme": "dark"
}
"""
    in
    D.decodeString (D.dict D.value) json |> Result.withDefault Dict.empty


suite : Test
suite =
    describe "Studio.Persist"
        [ describe "identity-key factory"
            [ test "same identity → same groups key" <|
                \_ -> Persist.groupsKey corpusA |> Expect.equal (Persist.groupsKey corpusA)
            , test "different identity → different keys (no cross-read)" <|
                \_ ->
                    Expect.equal False (Persist.groupsKey corpusA == Persist.groupsKey corpusB)
            , test "keys carry the artifact name and identity" <|
                \_ ->
                    Persist.groupsKey corpusA
                        |> Expect.equal ("graphos-studio:groups:" ++ corpusA)
            ]
        , describe "filterFor A/B isolation"
            [             test "reading corpus B returns only B's per-graph keys" <|
                \_ ->
                    Dict.keys (Persist.filterFor corpusB both)
                        |> List.sort
                        |> Expect.equal
                            [ Persist.editlogKey corpusB
                            , Persist.groupsKey corpusB
                            , Persist.tuningKey corpusB
                            ]
            , test "corpus A's bytes stay stored under its own key (not deleted)" <|
                \_ ->
                    Dict.member (Persist.groupsKey corpusA) both
                        |> Expect.equal True
            , test "unrelated keys (theme) are never read as per-graph state" <|
                \_ ->
                    Dict.member "graphos-studio:theme" (Persist.filterFor corpusB both)
                        |> Expect.equal False
            ]
        ]
