module Dictionary
    exposing
        ( Dictionary
        , DictionaryResponse
        , decodeDictionary
        , dictionaryFromResponse
        )

import Dict exposing (Dict)
import Json.Decode
import Json.Decode.Pipeline


type alias Dictionary =
    Dict String ()


type alias DictionaryResponse =
    { dictionary : List String
    }


decodeDictionary : Json.Decode.Decoder DictionaryResponse
decodeDictionary =
    Json.Decode.Pipeline.decode DictionaryResponse
        |> Json.Decode.Pipeline.required "dictionary" (Json.Decode.list Json.Decode.string)


dictionaryFromResponse : List String -> Dictionary
dictionaryFromResponse response =
    List.map (\word -> ( word, () )) response
        |> Dict.fromList
