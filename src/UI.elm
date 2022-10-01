module UI exposing
    ( button
    , largeButton
    , letterButton
    , selectedLetterButton
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes


unselectable : Element.Attribute msg
unselectable =
    Element.htmlAttribute (Html.Attributes.attribute "user-select" "none")


buttonCommon : List (Element.Attribute msg)
buttonCommon =
    [ unselectable
    , Background.color pink
    , Font.color white
    , Element.pointer
    , Element.paddingXY 8 8
    , Border.color pink
    , Border.rounded 4
    ]


transparent : Element.Color
transparent =
    Element.rgba255 0 0 0 0


black : Element.Color
black =
    Element.rgb255 0 0 0


white : Element.Color
white =
    Element.rgb255 255 255 255


pink : Element.Color
pink =
    Element.rgb255 255 80 80


button : List (Element.Attribute msg) -> Element msg -> Element msg
button attrs =
    Element.el (buttonCommon ++ attrs)


largeButton : List (Element.Attribute msg) -> Element msg -> Element msg
largeButton attrs =
    Element.el (buttonCommon ++ attrs)


letterCommon : List (Element.Attribute msg)
letterCommon =
    -- line height is handled by element.spacing
    [ Element.pointer
    , Border.rounded 12
    , Font.size 30
    , Font.bold
    , Element.centerX
    , Element.paddingXY 8 4
    , Element.width (Element.px 96)
    ]


letterButton : List (Element.Attribute msg) -> Element msg -> Element msg
letterButton attrs =
    [ letterCommon
    , [ Background.color white, Font.color pink ]
    , attrs
    ]
        |> List.concat
        |> Element.el


selectedLetterButton : List (Element.Attribute msg) -> Element msg -> Element msg
selectedLetterButton attrs =
    [ letterCommon
    , [ Background.color pink, Font.color white ]
    , attrs
    ]
        |> List.concat
        |> Element.el
