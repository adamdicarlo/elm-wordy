module Update exposing (keyCodeToCmd, update)

import Char
import List.Extra exposing (findIndex)
import Keyboard
import Dict exposing (Dict)
import Dictionary exposing (Dictionary, dictionaryFromResponse)
import Letter exposing (..)
import Model exposing (Model, Msg(..), guessToString)
import RemoteData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ game } as model) =
    case msg of
        DictionaryResponse response ->
            case response of
                RemoteData.Success data ->
                    { model
                        | game =
                            { game
                                | dictionary =
                                    RemoteData.Success (dictionaryFromResponse data.dictionary)
                            }
                    }
                        ! []

                RemoteData.Failure err ->
                    { model
                        | game =
                            { game
                                | dictionary = RemoteData.Failure err
                            }
                    }
                        ! []

                _ ->
                    Debug.crash "Unexpected RemoteData response"

        AddLetter ch index ->
            { model
                | game =
                    { game
                        | reverseGuess = ( ch, index ) :: game.reverseGuess
                        , letters = markAtIndex index game.letters
                    }
            }
                ! []

        Backspace ->
            case game.reverseGuess of
                ( ch, index ) :: ls ->
                    { model
                        | game =
                            { game
                                | reverseGuess = ls
                                , letters = unmarkAtIndex index game.letters
                            }
                    }
                        ! []

                [] ->
                    model ! []

        SubmitGuess ->
            let
                guess =
                    guessToString game.reverseGuess

                newFoundWords =
                    case game.dictionary of
                        RemoteData.Success dictionary ->
                            if validWord guess dictionary && eligibleWord guess game.foundWords then
                                guess :: game.foundWords
                            else
                                game.foundWords

                        _ ->
                            Debug.crash "No dictionary"
            in
                { model
                    | game =
                        { game
                            | reverseGuess = []
                            , letters = unmarkAll game.letters
                            , foundWords = newFoundWords
                        }
                }
                    ! []

        NewGame ->
            { model
                | screen = Model.Game
                , game =
                    { game
                        | letters = stringToLetterList "efinerrgt"
                        , reverseGuess = []
                        , foundWords = []
                    }
            }
                ! []

        NoOp ->
            model ! []


stringToLetterList : String -> List Letter
stringToLetterList str =
    case String.uncons str of
        Nothing ->
            []

        Just ( head, tail ) ->
            Letter (Char.toLower head) False :: stringToLetterList tail


findUnselectedLetter : List Letter -> Char -> Maybe Int
findUnselectedLetter letters sought =
    let
        predicate (Letter ch selected) =
            ch == sought && not selected
    in
        findIndex predicate letters


validWord : String -> Dictionary -> Bool
validWord word dictionary =
    Dict.member word dictionary


eligibleWord : String -> List String -> Bool
eligibleWord word foundWords =
    not (List.member word foundWords)


markAtIndex : Int -> List Letter -> List Letter
markAtIndex index letters =
    case letters of
        ((Letter ch _) as letter) :: rest ->
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
        ((Letter ch _) as letter) :: rest ->
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
            Letter ch False :: unmarkAll rest

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
                    Char.fromCode keyCode |> Char.toLower
            in
                case findUnselectedLetter model.game.letters ch of
                    Just index ->
                        AddLetter ch index

                    Nothing ->
                        NoOp
