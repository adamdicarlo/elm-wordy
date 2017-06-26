module Model exposing (..)

import Dict
import Http
import RemoteData exposing (..)
import Letter exposing (..)
import List.Extra exposing (remove)
import Dictionary exposing (Dictionary, DictionaryResponse, decodeDictionary)


type Screen
    = Menu
    | Game


type alias GameModel =
    { dictionary : WebData Dictionary
    , letters : List Letter
    , reverseGuess : List ( Char, Int )
    , foundWords : List String
    , totalWords : Int
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
      -- RNG response: Numbers used for picking a 9-letter word, then shuffling it, when
      -- starting a new game
    | NewGameNumbers ( Int, List Int )
    | NoOp


init : ( Model, Cmd Msg )
init =
    { screen = Menu
    , game =
        { dictionary = Loading
        , letters = []
        , reverseGuess = []
        , foundWords = []
        , totalWords = 0
        }
    }
        ! [ getDictionary ]


fallbackWord : String
fallbackWord =
    "flowering"


getDictionary : Cmd Msg
getDictionary =
    Http.get "/dictionary.json" decodeDictionary
        |> RemoteData.sendRequest
        |> Cmd.map DictionaryResponse


getWords : WebData Dictionary -> Dictionary
getWords dictionary =
    let
        fallback : Dictionary
        fallback =
            Dict.singleton fallbackWord ()
    in
        RemoteData.toMaybe dictionary
            |> Maybe.withDefault fallback


guessToString : List ( Char, Int ) -> String
guessToString cs =
    List.map (\t -> Tuple.first t) cs
        |> String.fromList
        |> String.reverse


isWordInBoard : String -> List Char -> Bool
isWordInBoard word boardChars =
    areCharsInBoard (String.toList word) boardChars


areCharsInBoard : List Char -> List Char -> Bool
areCharsInBoard word boardChars =
    case word of
        [] ->
            True

        ch :: restChars ->
            let
                restBoardChars =
                    remove ch boardChars
            in
                -- If we weren't able to remove the current char, it wasn't there, thus the given
                -- word is not contained in the chars on the board
                if List.length restBoardChars == List.length boardChars then
                    False
                else
                    areCharsInBoard restChars restBoardChars


lettersToCharList : List Letter -> List Char
lettersToCharList letters =
    List.map (\(Letter char _) -> char) letters


stringToLetterList : String -> List Letter
stringToLetterList =
    String.foldr (\char accum -> Letter char False :: accum) []


totalWords : WebData Dictionary -> List Letter -> Int
totalWords dictionary letters =
    let
        charList =
            lettersToCharList letters

        predicate word _ =
            isWordInBoard word charList
    in
        getWords dictionary
            |> Dict.filter predicate
            |> Dict.size
