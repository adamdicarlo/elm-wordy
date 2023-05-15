module UI exposing
    ( button
    , largeButton
    , letterButton
    , pink
    , selectedLetterButton
    , white
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attributes


unselectable : Element.Attribute msg
unselectable =
    Element.htmlAttribute (Attributes.attribute "user-select" "none")


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


white : Element.Color
white =
    Element.rgb255 255 255 255


pink : Element.Color
pink =
    Element.rgb255 255 80 80


button : List (Element.Attribute msg) -> { label : Element msg, onPress : Maybe msg } -> Element msg
button attrs { label, onPress } =
    Element.el
        ([ buttonCommon
         , attrs
         ]
            |> List.concat
            |> withMaybeOnPress onPress
        )
        label


largeButton : List (Element.Attribute msg) -> { label : Element msg, onPress : Maybe msg } -> Element msg
largeButton attrs =
    Input.button
        ([ unselectable
         , Background.color pink
         , Font.color white
         , Font.size 48
         , Font.family [ Font.monospace ]
         , Element.paddingXY 30 22
         , Border.color pink
         , Border.shadow
            { offset = ( 0, 0 )
            , size = 1.0
            , blur = 8.0
            , color = Element.rgb 0.5 0.5 0.5
            }
         , Border.rounded 4
         , Element.mouseDown
            [ Border.shadow
                { offset = ( 0, 0 )
                , size = 0
                , blur = 0
                , color = Element.rgb 0 0 0
                }
            ]
         ]
            ++ attrs
        )


withMaybeOnPress : Maybe msg -> List (Element.Attribute msg) -> List (Element.Attribute msg)
withMaybeOnPress maybeOnPress attrs =
    case maybeOnPress of
        Just onPress ->
            Events.onClick onPress :: attrs

        Nothing ->
            attrs


letterCommon : Element.Color -> Element.Color -> Maybe msg -> List (Element.Attribute msg)
letterCommon bgColor fontColor maybeOnPress =
    -- line height is handled by element.spacing
    [ Attributes.style "transition" "all 0.1s"
        |> Element.htmlAttribute
    , Background.color bgColor
    , Element.width (Element.px 100)
    , Element.height (Element.px 100)
    , Element.pointer
    , Border.rounded 12
    , Font.color fontColor
    , Font.family [ Font.monospace ]
    , Font.size 48
    , Font.bold
    ]
        |> withMaybeOnPress maybeOnPress


letterButton : { letter : String, onPress : Maybe msg } -> Element msg
letterButton { letter, onPress } =
    Element.el
        (letterCommon white pink onPress)
        (Element.el
            [ Element.centerX
            , Element.centerY
            ]
            (Element.text letter)
        )


selectedLetterButton : { letter : String, onPress : Maybe msg } -> Element msg
selectedLetterButton { letter, onPress } =
    Element.el
        (Element.scale 0.85 :: letterCommon pink white onPress)
        (Element.el
            [ Element.centerX
            , Element.centerY
            ]
            (Element.text letter)
        )
