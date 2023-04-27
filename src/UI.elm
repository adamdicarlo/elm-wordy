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
import Element.Input as Input
import Html.Attributes


unselectable : Element.Attribute msg
unselectable =
    Element.htmlAttribute (Html.Attributes.attribute "user-select" "none")


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


button : List (Element.Attribute msg) -> { label : Element msg, onPress : Maybe msg } -> Element msg
button attrs =
    [ unselectable
    , Background.color pink
    , Font.color white
    , Element.pointer
    , Element.paddingXY 8 8
    , Border.color pink
    , Border.rounded 4
    ]
        ++ attrs
        |> Input.button


largeButton : List (Element.Attribute msg) -> { label : Element msg, onPress : Maybe msg } -> Element msg
largeButton attrs =
    [ unselectable
    , Background.color pink
    , Font.color white
    , Font.size 30
    , Element.pointer
    , Element.paddingXY 20 12
    , Border.color pink
    , Border.rounded 4
    ]
        ++ attrs
        |> Input.button


letterCommon : List (Element.Attribute msg)
letterCommon =
    -- line height is handled by element.spacing
    [ Element.pointer
    , Element.width (Element.px 100)
    , Element.height (Element.px 100)
    , Border.rounded 12
    , Font.bold
    , Font.center
    , Font.size 48
    ]


letterButton : List (Element.Attribute msg) -> { label : Element msg, onPress : Maybe msg } -> Element msg
letterButton attrs =
    [ letterCommon
    , [ Background.color white, Font.color pink ]
    , attrs
    ]
        |> List.concat
        |> Input.button


selectedLetterButton : List (Element.Attribute msg) -> { label : Element msg, onPress : Maybe msg } -> Element msg
selectedLetterButton attrs =
    [ letterCommon
    , [ Background.color pink, Font.color white ]
    , attrs
    ]
        |> List.concat
        |> Input.button
