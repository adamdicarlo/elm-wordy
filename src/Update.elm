module Update exposing (keyCodeToCmd, update)

import Char
import List.Extra exposing (findIndex)
import Keyboard
import Letter exposing (..)
import Model exposing (Model, Msg(..), guessToString)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddLetter ch index ->
            { model
                | reverseGuess = ( ch, index ) :: model.reverseGuess
                , letters = markAtIndex index model.letters
            }
                ! []

        Backspace ->
            case model.reverseGuess of
                ( ch, index ) :: ls ->
                    { model
                        | reverseGuess = ls
                        , letters = unmarkAtIndex index model.letters
                    }
                        ! []

                [] ->
                    model ! []

        SubmitGuess ->
            let
                guess =
                    guessToString model.reverseGuess

                newFoundWords =
                    if validWord guess model.dictionary && eligibleWord guess model.foundWords then
                        guess :: model.foundWords
                    else
                        model.foundWords
            in
                { model
                    | reverseGuess = []
                    , letters = unmarkAll model.letters
                    , foundWords = newFoundWords
                }
                    ! []

        NoOp ->
            model ! []


findUnselectedLetter : List Letter -> Char -> Maybe Int
findUnselectedLetter letters sought =
    let
        predicate (Letter ch selected) =
            ch == sought && not selected
    in
        findIndex predicate letters


validWord : String -> List String -> Bool
validWord word dictionary =
    List.member word dictionary


eligibleWord : String -> List String -> Bool
eligibleWord word foundWords =
    not (List.member word foundWords)


markAtIndex : Int -> List Letter -> List Letter
markAtIndex index letters =
    case letters of
        ((Letter ch selected) as letter) :: rest ->
            (if index == 0 then
                Letter ch True
             else
                letter
            )
                :: markAtIndex (index - 1) rest

        [] ->
            []


unmarkAtIndex : Int -> List Letter -> List Letter
unmarkAtIndex index letters =
    case letters of
        ((Letter ch selected) as letter) :: rest ->
            (if index == 0 then
                Letter ch False
             else
                letter
            )
                :: unmarkAtIndex (index - 1) rest

        [] ->
            []


unmarkAll : List Letter -> List Letter
unmarkAll letters =
    case letters of
        (Letter ch _) :: rest ->
            (Letter ch False) :: unmarkAll rest

        [] ->
            []


keyCodeToCmd : Model -> Keyboard.KeyCode -> Msg
keyCodeToCmd model keyCode =
    case keyCode of
        -- Enter key
        13 ->
            SubmitGuess

        -- Backspace key
        8 ->
            Backspace

        _ ->
            let
                ch =
                    Char.fromCode keyCode |> Char.toUpper
            in
                case findUnselectedLetter model.letters ch of
                    Just index ->
                        AddLetter ch index

                    Nothing ->
                        NoOp
