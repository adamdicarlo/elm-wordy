module Main exposing (main)

import Html exposing (Html)
import App exposing (Model, Msg, init, subscriptions, update, view)


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }
