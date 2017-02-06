module Wordy exposing (main)

import Html exposing (Html, div, h1, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


dictionary : List String
dictionary =
    [ "ART"
    , "DON"
    , "DART"
    , "DRAT"
    , "DRAW"
    , "DRAWN"
    , "DROWN"
    , "DOWN"
    , "HARD"
    , "HORN"
    , "HOT"
    , "HOW"
    , "NOD"
    , "NOR"
    , "NORTH"
    , "NORTHWARD"
    , "NOT"
    , "RANT"
    , "RAW"
    , "ROD"
    , "ROW"
    , "THAN"
    , "THAW"
    , "THROW"
    , "THROWN"
    , "TROD"
    , "WAD"
    , "WANT"
    , "WAR"
    , "WARD"
    , "WART"
    , "WHO"
    , "WHOA"
    , "WONT"
    , "WORD"
    , "WORTH"
    ]


type Letter
    = Letter Char Bool


type alias Model =
    { letters : List Letter
    , reverseGuess : List ( Char, Int )
    , foundWords : List String
    }


model : Model
model =
    { letters = stringToLetterList "dwnaorthr"
    , reverseGuess = []
    , foundWords = []
    }


type Msg
    = AddLetter Char Int
    | Backspace
    | SubmitGuess


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
                    if validWord guess && eligibleWord guess model.foundWords then
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


validWord : String -> Bool
validWord word =
    List.member word dictionary


eligibleWord : String -> List String -> Bool
eligibleWord word foundWords =
    not (List.member word foundWords)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Wordy" ]
        , viewLetters model.letters
        , div [ class "guess" ] [ text <| guessToString model.reverseGuess ]
        , div []
            [ button [ onClick Backspace ] [ text "Backspace" ]
            , button [ onClick SubmitGuess ] [ text "Submit word" ]
            ]
        , viewFoundWords model.foundWords
        ]


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


stringToLetterList : String -> List Letter
stringToLetterList str =
    case String.uncons str of
        Nothing ->
            []

        Just ( head, tail ) ->
            Letter head False :: stringToLetterList tail


guessToString : List ( Char, Int ) -> String
guessToString cs =
    List.map (\t -> Tuple.first t) cs
        |> String.fromList
        |> String.reverse
        |> String.toUpper


viewLetters : List Letter -> Html Msg
viewLetters letters =
    div [ class "letters" ] (List.indexedMap viewLetter letters)


viewLetter : Int -> Letter -> Html Msg
viewLetter index (Letter ch selected) =
    let
        attrs =
            [ class "letter" ]
                ++ if selected then
                    [ class "selected" ]
                   else
                    [ onClick (AddLetter ch index) ]
    in
        div attrs [ text (ch |> String.fromChar |> String.toUpper) ]


viewFoundWords : List String -> Html Msg
viewFoundWords foundWords =
    let
        children =
            List.map (\w -> Html.li [] [ text w ]) foundWords
    in
        Html.ol [ class "foundWords" ] children


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = model ! []
        }
