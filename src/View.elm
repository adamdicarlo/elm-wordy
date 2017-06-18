module View exposing (view)

import Html exposing (Html, div, h1, h2, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra
import RemoteData
import Letter exposing (..)
import Model exposing (..)


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
                    [ button [ onClick NewGame ] [ text "New Game" ] ]
    in
        div []
            ([ h1 [] [ text "Wordy" ] ] ++ content)


viewGame : GameModel -> Html Msg
viewGame game =
    div []
        [ h1 [] [ text "Wordy" ]
        , viewLetters game.letters
        , div [ class "guess" ] [ guessToString game.reverseGuess |> String.toUpper |> text ]
        , div []
            [ button [ onClick Backspace ] [ text "Backspace" ]
            , button [ onClick Shuffle ] [ text "Shuffle" ]
            , button [ onClick SubmitGuess ] [ text "Submit word" ]
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
            List.map (\w -> Html.li [] [ text <| String.toUpper w ]) foundWords
    in
        Html.ol [ class "foundWords" ] children
