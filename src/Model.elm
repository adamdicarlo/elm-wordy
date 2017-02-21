module Model exposing (..)

import Char
import List.Extra exposing (findIndex)
import Letter exposing (..)


type Screen
    = Menu
    | Game


type alias Model =
    { screen : Screen
    , dictionary : List String
    , letters : List Letter
    , reverseGuess : List ( Char, Int )
    , foundWords : List String
    }


type Msg
    = AddLetter Char Int
    | Backspace
    | SubmitGuess
    | NoOp


init : ( Model, Cmd Msg )
init =
    { screen = Menu
    , dictionary = []
    , letters = stringToLetterList "dwnaorthr"
    , reverseGuess = []
    , foundWords = []
    }
        ! []


guessToString : List ( Char, Int ) -> String
guessToString cs =
    List.map (\t -> Tuple.first t) cs
        |> String.fromList
        |> String.reverse
        |> String.toUpper


stringToLetterList : String -> List Letter
stringToLetterList str =
    case String.uncons str of
        Nothing ->
            []

        Just ( head, tail ) ->
            Letter (Char.toUpper head) False :: stringToLetterList tail
