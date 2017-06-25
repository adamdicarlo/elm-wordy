module Update exposing (keyCodeToCmd, update)

import Char
import List.Extra exposing (findIndex)
import Keyboard
import Dict exposing (Dict)
import Dictionary exposing (Dictionary, dictionaryFromResponse)
import Letter exposing (..)
import Model exposing (Model, Msg(..), guessToString)
import Random exposing (Generator)
import RemoteData exposing (WebData)
import Tuple exposing (first, second)


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

        Shuffle ->
            let
                generator =
                    shuffleWordGenerator (List.length model.game.letters)
            in
                model ! [ Random.generate ShuffleOrdering generator ]

        ShuffleOrdering values ->
            { model | game = { game | letters = shuffle values game.letters } } ! []

        NewGame ->
            let
                generator =
                    Random.pair (selectWordGenerator game.dictionary) (shuffleWordGenerator 9)
            in
                model ! [ Random.generate NewGameNumbers generator ]

        NewGameNumbers ( wordIndex, shuffleNumbers ) ->
            let
                letters =
                    nineLetterWords game.dictionary
                        |> Dict.keys
                        |> List.drop wordIndex
                        |> List.head
                        |> Maybe.withDefault fallbackWord
                        |> stringToLetterList
                        |> shuffle shuffleNumbers
            in
                { model
                    | screen = Model.Game
                    , game =
                        { game
                            | letters = letters
                            , reverseGuess = []
                            , foundWords = []
                        }
                }
                    ! []

        NoOp ->
            model ! []


fallbackWord : String
fallbackWord =
    "flowering"


nineLetterWords : WebData Dictionary -> Dictionary
nineLetterWords dictionary =
    let
        fallback : Dictionary
        fallback =
            Dict.singleton fallbackWord ()

        predicate : String -> () -> Bool
        predicate word _ =
            String.length word == 9
    in
        RemoteData.toMaybe dictionary
            |> Maybe.withDefault fallback
            |> Dict.filter predicate


selectWordGenerator : WebData Dictionary -> Generator Int
selectWordGenerator dictionary =
    let
        nineLetterWordCount =
            Dict.size (nineLetterWords dictionary)
    in
        Random.int 0 (nineLetterWordCount - 1)


shuffleWordGenerator : Int -> Generator (List Int)
shuffleWordGenerator wordLength =
    Random.list wordLength (Random.int 0 100)


shuffle : List Int -> List Letter -> List Letter
shuffle randoms letters =
    let
        zipped : List ( Int, Letter )
        zipped =
            List.map2 (,) randoms letters

        sorted =
            List.sortBy first zipped
    in
        List.unzip sorted |> second


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
