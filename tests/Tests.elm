module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Update exposing (update)
import Model exposing (guessToString, init, GameModel, Model, Msg(..))


start =
    Tuple.first init


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
        ]
