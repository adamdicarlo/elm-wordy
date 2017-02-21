module View exposing (view)

import Html exposing (Html, div, h1, h2, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra exposing (findIndex)
import Letter exposing (..)
import Model exposing (..)


view : Model -> Html Msg
view model =
    case model.screen of
        Menu ->
            viewMenu model

        Game ->
            viewGame model


viewMenu model =
    div []
        [ h1 [] [ text "Wordy" ]
        , h2 [] [ text "Loading..." ]
        ]


viewGame model =
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


viewLetters : List Letter -> Html Msg
viewLetters letters =
    let
        rows =
            letters
                |> List.indexedMap viewLetter
                |> List.Extra.groupsOf 3
    in
        -- Wrap each group (3 letters) into a "row" div
        div [ class "letters" ] <| List.map (div [ class "letter-row" ]) rows


viewLetter : Int -> Letter -> Html Msg
viewLetter index (Letter ch selected) =
    let
        extraAttr =
            if selected then
                [ class "selected" ]
            else
                [ onClick (AddLetter ch index) ]
    in
        div ([ class "letter" ] ++ extraAttr) [ text (ch |> String.fromChar |> String.toUpper) ]


viewFoundWords : List String -> Html Msg
viewFoundWords foundWords =
    let
        children =
            List.map (\w -> Html.li [] [ text w ]) foundWords
    in
        Html.ol [ class "foundWords" ] children
