module Dictionary exposing
    ( Dictionary
    , decode
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


{-| The set of legal words.

A Set would make more sense for this than a Dict (ironically). But a set with more than around 3550
elements causes the Elm debugger to crash. <https://github.com/elm/browser/issues/132>

-}
type alias Dictionary =
    Dict String ()


decode : Decoder Dictionary
decode =
    Decode.succeed
        (List.map
            (\word -> Tuple.pair word ())
            >> Dict.fromList
        )
        |> Decode.required "dictionary" (Decode.list Decode.string)
