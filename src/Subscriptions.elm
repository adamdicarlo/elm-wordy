module Subscriptions exposing (..)

import Keyboard
import Model exposing (Model, Msg)
import Update exposing (keyCodeToCmd)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs <| keyCodeToCmd model
        ]
