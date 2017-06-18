module Model exposing (..)

import Http
import RemoteData exposing (..)
import Letter exposing (..)
import Dictionary exposing (Dictionary, DictionaryResponse, decodeDictionary)


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
    = -- User action: Adds the letter (Char) from a position (Int) to the guess
      AddLetter Char Int
      -- User action: Erase last letter of guess
    | Backspace
      -- User action: Submit the guess
    | SubmitGuess
      -- User action: Shuffle the board
    | Shuffle
      -- User action: Start a new game
    | NewGame
      -- AJAX response for dictionary (uses RemoteData package)
    | DictionaryResponse (WebData DictionaryResponse)
      -- RNG response: Numbers used for shuffling letters
    | ShuffleOrdering (List Int)
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
