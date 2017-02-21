module Model exposing (..)

import Http
import RemoteData exposing (..)
import Letter exposing (..)
import Dictionary exposing (..)


type Screen
    = Menu
    | Game


type alias GameModel =
    { dictionary : WebData Dictionary
    , letters : List Letter
    , reverseGuess : List ( Char, Int )
    , foundWords : List String
    }


type alias Model =
    { screen : Screen
    , game : GameModel
    }


type Msg
    = AddLetter Char Int
    | Backspace
    | SubmitGuess
    | DictionaryResponse (WebData DictionaryResponse)
    | NewGame
    | NoOp


init : ( Model, Cmd Msg )
init =
    { screen = Menu
    , game =
        { dictionary = Loading
        , letters = []
        , reverseGuess = []
        , foundWords = []
        }
    }
        ! [ getDictionary ]


getDictionary : Cmd Msg
getDictionary =
    Http.get "/dictionary.json" decodeDictionary
        |> RemoteData.sendRequest
        |> Cmd.map DictionaryResponse


guessToString : List ( Char, Int ) -> String
guessToString cs =
    List.map (\t -> Tuple.first t) cs
        |> String.fromList
        |> String.reverse
