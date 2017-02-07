module Wordy exposing (main)

import Html exposing (Html, div, h1, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Char
import List.Extra exposing (findIndex)
import Keyboard


type Letter
    = Letter Char Bool


type alias Model =
    { letters : List Letter
    , reverseGuess : List ( Char, Int )
    , foundWords : List String
    }


type Msg
    = AddLetter Char Int
    | Backspace
    | SubmitGuess
    | NoOp


dictionary : List String
dictionary =
    [ "ADO"
    , "ADORN"
    , "ADOWN"
    , "AND"
    , "ANT"
    , "ARDOR"
    , "ARROW"
    , "ART"
    , "AWN"
    , "DAH"
    , "DARN"
    , "DART"
    , "DATO"
    , "DAW"
    , "DAWN"
    , "DAWT"
    , "DHOW"
    , "DOAT"
    , "DON"
    , "DONA"
    , "DOR"
    , "DORR"
    , "DOT"
    , "DOTH"
    , "DOW"
    , "DOWN"
    , "DRAT"
    , "DRAW"
    , "DRAWN"
    , "DROWN"
    , "HAD"
    , "HADRON"
    , "HAND"
    , "HANT"
    , "HAO"
    , "HARD"
    , "HARROW"
    , "HART"
    , "HAT"
    , "HAW"
    , "HOAR"
    , "HOARD"
    , "HOD"
    , "HON"
    , "HONDA"
    , "HORA"
    , "HORN"
    , "HOT"
    , "HOW"
    , "HWAN"
    , "NAH"
    , "NARD"
    , "NARROW"
    , "NAW"
    , "NOD"
    , "NOH"
    , "NOR"
    , "NORTH"
    , "NORTHWARD"
    , "NOT"
    , "NOTA"
    , "NOW"
    , "NOWT"
    , "NTH"
    , "OAR"
    , "OAT"
    , "OATH"
    , "ONWARD"
    , "ORA"
    , "ORAD"
    , "ORRA"
    , "ORT"
    , "OWN"
    , "RAD"
    , "RADON"
    , "RAH"
    , "RAN"
    , "RAND"
    , "RANT"
    , "RAT"
    , "RATH"
    , "RATO"
    , "RAW"
    , "RHO"
    , "ROAD"
    , "ROAN"
    , "ROAR"
    , "ROD"
    , "ROT"
    , "ROTA"
    , "ROW"
    , "ROWAN"
    , "ROWTH"
    , "TAD"
    , "TAHR"
    , "TAN"
    , "TAO"
    , "TAR"
    , "TARDO"
    , "TARN"
    , "TARO"
    , "TAW"
    , "THAN"
    , "THAW"
    , "THO"
    , "THORN"
    , "THRAW"
    , "THRAWN"
    , "THRO"
    , "THROW"
    , "THROWN"
    , "TOAD"
    , "TOD"
    , "TON"
    , "TOR"
    , "TORA"
    , "TORAH"
    , "TORN"
    , "TORR"
    , "TOW"
    , "TOWARD"
    , "TOWN"
    , "TRAD"
    , "TROD"
    , "TRONA"
    , "TROW"
    , "TWA"
    , "TWO"
    , "WAD"
    , "WAN"
    , "WAND"
    , "WANT"
    , "WAR"
    , "WARD"
    , "WARN"
    , "WART"
    , "WAT"
    , "WHA"
    , "WHAT"
    , "WHO"
    , "WHOA"
    , "WHORT"
    , "WOAD"
    , "WON"
    , "WONT"
    , "WORD"
    , "WORN"
    , "WORT"
    , "WORTH"
    , "WOT"
    , "WRATH"
    , "WROTH"
    ]


initialModel : Model
initialModel =
    { letters = stringToLetterList "dwnaorthr"
    , reverseGuess = []
    , foundWords = []
    }


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

        NoOp ->
            model ! []


findUnselectedLetter : List Letter -> Char -> Maybe Int
findUnselectedLetter letters sought =
    let
        predicate (Letter ch selected) =
            ch == sought && not selected
    in
        findIndex predicate letters


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
            Letter (Char.toUpper head) False :: stringToLetterList tail


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
        , subscriptions = subscriptions
        , init = initialModel ! []
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs <| keyCodeToCmd model
        ]


keyCodeToCmd : Model -> Keyboard.KeyCode -> Msg
keyCodeToCmd model keyCode =
    case (Debug.log "keyCode" keyCode) of
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
