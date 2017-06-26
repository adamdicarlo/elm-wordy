module View exposing (view)

import Html exposing (Html, div, h1, h2, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import List.Extra
import RemoteData exposing (WebData)
import Letter exposing (..)
import Model exposing (GameModel, Model, Msg(..), guessToString)
import UI exposing (button, largeButton, letterButton, selectedLetterButton)


view : Model -> Html Msg
view model =
    case model.screen of
        Model.Menu ->
            viewMenu model

        Model.Game ->
            viewGame model.game


viewMenu : Model -> Html Msg
viewMenu model =
    let
        content =
            case model.game.dictionary of
                RemoteData.NotAsked ->
                    [ h2 [] [ text "Starting" ] ]

                RemoteData.Loading ->
                    [ h2 [] [ text "Loading..." ] ]

                RemoteData.Failure err ->
                    [ text ("Error: " ++ toString err) ]

                RemoteData.Success _ ->
                    [ largeButton [ onClick NewGame ] [ text "New Game" ] ]
    in
        div []
            ([ h1 [] [ text "Wordy" ] ] ++ content)


viewGame : GameModel -> Html Msg
viewGame game =
    div []
        [ h1 [] [ text "Wordy" ]
        , viewLetters game.letters
        , div [ class "guess" ] [ guessToString game.reverseGuess |> String.toUpper |> text ]
        , div [ class "primaryButtons" ]
            [ button [ onClick Backspace ] [ text "Backspace" ]
            , button [ onClick Shuffle ] [ text "Shuffle" ]
            , button [ onClick SubmitGuess ] [ text "Submit word" ]
            ]
        , div []
            [ div [] [ text (toString (List.length game.foundWords) ++ " found") ]
            , div [] [ text (toString game.totalWords ++ " total") ]
            ]
        , viewFoundWords game.foundWords
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
        div [ class "letterGrid", attribute "unselectable" "on" ] <| List.map (div [ class "letterRow" ]) rows


viewLetter : Int -> Letter -> Html Msg
viewLetter index (Letter ch selected) =
    let
        element =
            if selected then
                selectedLetterButton []
            else
                letterButton [ onClick (AddLetter ch index) ]
    in
        element [ text (ch |> String.fromChar |> String.toUpper) ]


viewFoundWords : List String -> Html Msg
viewFoundWords foundWords =
    let
        children =
            List.map (\w -> Html.li [] [ text <| String.toUpper w ]) foundWords
    in
        Html.ol [ class "foundWords" ] children
