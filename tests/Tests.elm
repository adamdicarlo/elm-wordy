module Tests exposing (all, dictionary, start, updateModel, withReverseGuess)

import Dict
import Expect
import Letter exposing (Letter(..))
import Model exposing (GameModel, Model, Msg(..), guessToString, init, isWordInBoard, stringToLetterList, totalWords)
import RemoteData exposing (WebData)
import Test exposing (..)
import Update exposing (update)


start =
    Tuple.first init


dictionary =
    -- Four words that match 9-letter "flowering" (including "flowering" itself)
    Dict.singleton "flowering" ()
        |> Dict.insert "flow" ()
        |> Dict.insert "wolf" ()
        |> Dict.insert "lower" ()
        -- and some words that don't.
        |> Dict.insert "hay" ()
        |> Dict.insert "success" ()
        |> Dict.insert "partition" ()
        |> RemoteData.Success


updateModel : Msg -> Model -> Model
updateModel msg model =
    update msg model |> Tuple.first


withReverseGuess : List ( Char, Int ) -> Model
withReverseGuess reverseGuess =
    let
        startGame =
            start.game
    in
    { start
        | game =
            { startGame
                | reverseGuess = reverseGuess
            }
    }


all : Test
all =
    describe "Wordy"
        [ test "AddLetter adds a letter to the guess" <|
            \() ->
                Expect.equal
                    (start
                        |> updateModel (AddLetter 'a' 5)
                        |> .game
                        |> .reverseGuess
                    )
                    [ ( 'a', 5 ) ]
        , test "AddLetter twice retains both letters in guess, in reverse order" <|
            \() ->
                Expect.equal
                    (start
                        |> updateModel (AddLetter 'a' 5)
                        |> updateModel (AddLetter 'd' 4)
                        |> .game
                        |> .reverseGuess
                    )
                    [ ( 'd', 4 ), ( 'a', 5 ) ]
        , test "Backspace deletes most recent letter of guess" <|
            \() ->
                Expect.equal
                    (withReverseGuess [ ( 'd', 4 ), ( 'a', 5 ) ]
                        |> updateModel Backspace
                        |> .game
                        |> .reverseGuess
                    )
                    [ ( 'a', 5 ) ]
        , test "Backspace has no effect when guess is empty" <|
            \() ->
                Expect.equal
                    (start
                        |> updateModel Backspace
                        |> .game
                        |> .reverseGuess
                    )
                    []
        , test "guessToString" <|
            \() ->
                Expect.equal (guessToString [ ( 'd', 4 ), ( 'a', 5 ) ]) "ad"
        , test "guessToString with empty guess" <|
            \() ->
                Expect.equal (guessToString []) ""
        , test "isWordInBoard 'wolf' 'flowering'" <|
            \() ->
                Expect.equal (isWordInBoard "wolf" (String.toList "flowering")) True
        , test "isWordInBoard 'cowering' 'flowering'" <|
            \() ->
                Expect.equal (isWordInBoard "cowering" (String.toList "flowering")) False
        , test "isWordInBoard 'floweringf' 'flowering'" <|
            \() ->
                Expect.equal (isWordInBoard "floweringf" (String.toList "flowering")) False
        , test "isWordInBoard 'grew' 'flowering'" <|
            \() ->
                Expect.equal (isWordInBoard "grew" (String.toList "flowering")) True
        , test "totalWords" <|
            \() ->
                let
                    letters =
                        stringToLetterList "flowering"
                in
                Expect.equal (totalWords dictionary letters) 4
        ]
