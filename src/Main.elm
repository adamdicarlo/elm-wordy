module Main exposing (main)

import Html exposing (Html)
import Model exposing (Model, Msg, init)
import Subscriptions exposing (subscriptions)
import Update exposing (update)
import View exposing (view)


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }
