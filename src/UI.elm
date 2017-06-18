module UI
    exposing
        ( button
        , largeButton
        , letterButton
        , selectedLetterButton
        )

import Html exposing (Html, Attribute, a)
import Styled exposing (..)
import Styled.Colors exposing (black, pink, white)
import Styled.Cursors exposing (pointer)
import Styled.Selectors exposing (active, hover)
import Styled.Types


buttonCommon : Styled.Types.Rule
buttonCommon =
    mixin
        [ unselectable
        , backgroundColor pink
        , color white
        , cursor pointer
        , margin (Styled.em 0.5)
        , padding2 (Styled.em 0.25) (Styled.em 1)
        , border (px 2) solid pink
        , borderRadius (px 4)
        , active
            [ boxShadow (px 0) (px 0) (px 0) (px -8) transparent ]
        , hover
            [ boxShadow (px 0) (px 0) (px 8) (px -3) black ]
        ]


button : List (Attribute msg) -> List (Html msg) -> Html msg
button =
    styled Html.a
        [ buttonCommon
        , fontSize (Styled.em 1)
        ]


largeButton : List (Attribute msg) -> List (Html msg) -> Html msg
largeButton =
    styled Html.a
        [ buttonCommon
        , fontSize (Styled.em 1.5)
        ]


letterCommon : Styled.Types.Rule
letterCommon =
    mixin
        [ unselectable
        , cursor pointer
        , borderRadius (px 12)
        , display inlineFlex
        , fontSize (Styled.em 4)
        , fontWeight (int 700)
        , justifyContent center
        , lineHeight (px 96)
        , margin (px 4)
        , width (px 96)
        ]


letterButton : List (Attribute msg) -> List (Html msg) -> Html msg
letterButton =
    styled Html.a
        [ letterCommon
        , backgroundColor white
        , color pink
        ]


selectedLetterButton : List (Attribute msg) -> List (Html msg) -> Html msg
selectedLetterButton =
    styled Html.a
        [ letterCommon
        , backgroundColor pink
        , color white
        ]


unselectable : Styled.Types.Rule
unselectable =
    mixin [ declaration "user-select" [ "none" ] ]
