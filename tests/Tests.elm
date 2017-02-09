module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import App


start =
    Tuple.first App.init


updateModel msg model =
    App.update msg model |> Tuple.first


all : Test
all =
    describe "Wordy"
        [ test "AddLetter adds a letter to the guess" <|
            \() ->
                Expect.equal
                    (updateModel (App.AddLetter 'a' 5) start
                        |> .reverseGuess
                    )
                    [ ( 'a', 5 ) ]
        , test "AddLetter twice retains both letters in guess, in reverse order" <|
            \() ->
                Expect.equal
                    (updateModel (App.AddLetter 'a' 5) start
                        |> updateModel (App.AddLetter 'd' 4)
                        |> .reverseGuess
                    )
                    [ ( 'd', 4 ), ( 'a', 5 ) ]
        , test "Backspace deletes most recent letter of guess" <|
            \() ->
                Expect.equal
                    (updateModel App.Backspace { start | reverseGuess = [ ( 'd', 4 ), ( 'a', 5 ) ] }
                        |> .reverseGuess
                    )
                    [ ( 'a', 5 ) ]
        , test "Backspace has no effect when guess is empty" <|
            \() ->
                Expect.equal
                    (updateModel App.Backspace start
                        |> .reverseGuess
                    )
                    []
        , test "guessToString" <|
            \() ->
                Expect.equal (App.guessToString [ ( 'd', 4 ), ( 'a', 5 ) ]) "AD"
        , test "guessToString with empty guess" <|
            \() ->
                Expect.equal (App.guessToString []) ""
        ]
