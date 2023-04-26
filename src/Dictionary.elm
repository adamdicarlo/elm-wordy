module Dictionary exposing
    ( Dictionary
    , DictionaryResponse
    , decodeDictionary
    , dictionaryFromResponse
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias Dictionary =
    Dict String ()


type alias DictionaryResponse =
    { dictionary : List String
    }


decodeDictionary : Decoder DictionaryResponse
decodeDictionary =
    Decode.succeed DictionaryResponse
        |> Decode.required "dictionary" (Decode.list Decode.string)


dictionaryFromResponse : List String -> Dictionary
dictionaryFromResponse response =
    List.map (\word -> ( word, () )) response
        |> Dict.fromList
