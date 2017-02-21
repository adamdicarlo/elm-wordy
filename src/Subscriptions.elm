module Subscriptions exposing (..)

import Keyboard
import Model exposing (Model, Msg)
import Update exposing (keyCodeToCmd)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.screen of
        Model.Game ->
            Sub.batch
                [ Keyboard.downs <| keyCodeToCmd model
                ]

        _ ->
            Sub.none
