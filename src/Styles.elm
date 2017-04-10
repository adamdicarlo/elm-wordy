module Styles exposing (..)

import Css exposing (..)
import Css.Colors exposing (..)
import Css.Elements exposing (html, body, h1, button, main_)
import Css.Namespace exposing (namespace)


type CssClasses
    = Guess
    | Letter
    | Letters
    | LetterRow
    | Selected
    | FoundWords


htmlBodyStyles =
    [ backgroundColor (hex "f7f7f7")
    , fontFamily sansSerif
    ]


css =
    (stylesheet << namespace "wordy")
        [ body htmlBodyStyles
        , html htmlBodyStyles
        , main_
            [ displayFlex
            , justifyContent center
            , marginTop (px 60)
            ]
        , h1
            [ fontSize (px 64)
            ]
        , button
            [ fontSize (px 22)
            , lineHeight (num 1.5)
            ]
        , class Guess
            [ lineHeight (num 1)
            , height (em 1)
            , margin2 (em 0.5) (em 0)
            ]
        , class Letter
            [ color red
            , display inlineFlex
            , fontSize (px 64)
            , fontWeight bold
            , justifyContent center
            , width (px 96)
            , lineHeight (px 96)
            , border3 (px 1) solid black
            , cursor pointer
            ]
        , class Selected
            [ backgroundColor red
            , color (hex "fff")
            , cursor default
            ]
        , class FoundWords
            [ listStyleType none
            , fontSize (px 24)
            ]
        ]
