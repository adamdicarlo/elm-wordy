module Main exposing (GameModel, Letter(..), Model, Msg(..), guessToString, init, isWordInBoard, main, stringToLetterList, totalWords, update)

import Browser
import Browser.Events
import Dict
import Dictionary exposing (Dictionary)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Keyed
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Phosphor
import Random exposing (Generator)
import Task
import Time
import Tuple
import UI


type alias Flags =
    Encode.Value


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.screen of
        Game ->
            Sub.batch
                [ Browser.Events.onKeyDown (keyEventToCmd model)
                ]

        _ ->
            Sub.none


type Screen
    = Menu
    | Game


type Letter
    = Letter Char Bool


type Feedback
    = AlreadyFound Time.Posix String
    | Idle
    | InvalidWord Time.Posix String
    | WordFound Time.Posix String
    | WordTooShort Time.Posix String


type alias GameModel =
    { feedback : Feedback
    , foundWords : List String
    , letters : List Letter
    , reverseGuess : List ( Char, Int )
    , totalWords : Int
    }


type alias Model =
    { screen : Screen
    , game : GameModel
    }


{-| TODO: Rename msgs to indicate action source (e.g., UserClickedBackspace)
-}
type Msg
    = -- User action: Adds the letter (Char) from a position (Int) to the guess
      AddLetter Char Int
      -- User action: Erase last letter of guess
    | Backspace
    | PlayerPressedSubmit
    | SubmitGuess Time.Posix
      -- User action: Shuffle the board
    | Shuffle
      -- User action: Start a new game
    | NewGame
      -- RNG response: Numbers used for shuffling letters
    | ShuffleOrdering (List Int)
      -- RNG response: Numbers used for picking a 9-letter word, then shuffling it, when
      -- starting a new game
    | NewGameNumbers ( Int, List Int )
    | NoOp


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        letters : List Letter
        letters =
            flags
                |> Decode.decodeValue
                    (Decode.field "letters" Decode.string
                        |> Decode.andThen
                            (\string ->
                                if String.length string == 9 then
                                    Decode.succeed string

                                else
                                    Decode.fail "Wrong length"
                            )
                    )
                |> Result.withDefault ""
                |> stringToLetterList
    in
    ( { screen = Menu
      , game =
            { feedback = Idle
            , foundWords = []
            , letters = letters
            , reverseGuess = []
            , totalWords = 0
            }
      }
    , Cmd.none
    )


fallbackWord : String
fallbackWord =
    "flowering"


guessToString : List ( Char, Int ) -> String
guessToString cs =
    List.map (\t -> Tuple.first t) cs
        |> String.fromList
        |> String.reverse


isWordInBoard : String -> List Char -> Bool
isWordInBoard word boardChars =
    areCharsInBoard (String.toList word) boardChars


areCharsInBoard : List Char -> List Char -> Bool
areCharsInBoard word boardChars =
    case word of
        [] ->
            True

        ch :: restChars ->
            let
                restBoardChars : List Char
                restBoardChars =
                    List.remove ch boardChars
            in
            -- If we weren't able to remove the current char, it wasn't there, thus the given
            -- word is not contained in the chars on the board
            if List.length restBoardChars == List.length boardChars then
                False

            else
                areCharsInBoard restChars restBoardChars


lettersToCharList : List Letter -> List Char
lettersToCharList letters =
    List.map (\(Letter char _) -> char) letters


stringToLetterList : String -> List Letter
stringToLetterList =
    String.foldr (\char accum -> Letter char False :: accum) []


totalWords : Dictionary -> List Letter -> Int
totalWords dictionary letters =
    let
        charList : List Char
        charList =
            lettersToCharList letters

        predicate word _ =
            isWordInBoard word charList
    in
    dictionary
        |> Dict.filter predicate
        |> Dict.size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ game } as model) =
    case msg of
        AddLetter ch index ->
            ( { model
                | game =
                    { game
                        | reverseGuess = ( ch, index ) :: game.reverseGuess
                        , letters = markAtIndex index game.letters
                    }
              }
            , Cmd.none
            )

        Backspace ->
            case game.reverseGuess of
                ( _, index ) :: ls ->
                    ( { model
                        | game =
                            { game
                                | reverseGuess = ls
                                , letters = unmarkAtIndex index game.letters
                            }
                      }
                    , Cmd.none
                    )

                [] ->
                    ( model
                    , Cmd.none
                    )

        PlayerPressedSubmit ->
            ( model
            , Task.perform SubmitGuess Time.now
            )

        SubmitGuess time ->
            let
                guess : String
                guess =
                    guessToString game.reverseGuess

                isValid : Bool
                isValid =
                    validWord guess Dictionary.words

                isAlreadyFound : Bool
                isAlreadyFound =
                    not <| eligibleWord guess game.foundWords

                ( newFoundWords, feedback ) =
                    case ( isValid, isAlreadyFound ) of
                        ( True, True ) ->
                            ( game.foundWords, AlreadyFound time guess )

                        ( True, False ) ->
                            ( guess :: game.foundWords, WordFound time guess )

                        ( False, _ ) ->
                            if String.length guess < 3 then
                                ( game.foundWords, WordTooShort time guess )

                            else
                                ( game.foundWords, InvalidWord time guess )
            in
            ( { model
                | game =
                    { game
                        | feedback = feedback
                        , foundWords = newFoundWords
                        , letters = unmarkAll game.letters
                        , reverseGuess = []
                    }
              }
            , Cmd.none
            )

        Shuffle ->
            let
                generator : Generator (List Int)
                generator =
                    shuffleWordGenerator (List.length model.game.letters)
            in
            ( model
            , Random.generate ShuffleOrdering generator
            )

        ShuffleOrdering values ->
            ( { model | game = { game | letters = shuffle values game.letters } }
            , Cmd.none
            )

        NewGame ->
            let
                generator : Generator ( Int, List Int )
                generator =
                    Random.pair selectWordGenerator (shuffleWordGenerator 9)
            in
            ( model
            , Random.generate NewGameNumbers generator
            )

        NewGameNumbers ( wordIndex, shuffleNumbers ) ->
            let
                letters : List Letter
                letters =
                    if List.isEmpty game.letters then
                        nineLetterWords
                            |> Dict.keys
                            |> List.getAt wordIndex
                            |> Maybe.withDefault fallbackWord
                            |> stringToLetterList
                            |> shuffle shuffleNumbers

                    else
                        game.letters
            in
            ( { model
                | screen = Game
                , game =
                    { game
                        | letters = letters
                        , totalWords = totalWords Dictionary.words letters
                        , reverseGuess = []
                        , foundWords = []
                    }
              }
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


nineLetterWords : Dictionary
nineLetterWords =
    let
        predicate : String -> () -> Bool
        predicate word _ =
            String.length word == 9
    in
    Dictionary.words
        |> Dict.filter predicate


selectWordGenerator : Generator Int
selectWordGenerator =
    let
        nineLetterWordCount : Int
        nineLetterWordCount =
            Dict.size nineLetterWords
    in
    Random.int 0 (nineLetterWordCount - 1)


shuffleWordGenerator : Int -> Generator (List Int)
shuffleWordGenerator wordLength =
    Random.list wordLength (Random.int 0 100)


shuffle : List Int -> List Letter -> List Letter
shuffle randoms letters =
    let
        zipped : List ( Int, Letter )
        zipped =
            List.map2 (\a b -> ( a, b )) randoms letters

        sorted : List ( Int, Letter )
        sorted =
            List.sortBy Tuple.first zipped
    in
    List.unzip sorted |> Tuple.second


findUnselectedLetter : List Letter -> Char -> Maybe Int
findUnselectedLetter letters sought =
    let
        predicate : Letter -> Bool
        predicate (Letter ch selected) =
            ch == sought && not selected
    in
    List.findIndex predicate letters


validWord : String -> Dictionary -> Bool
validWord word dictionary =
    Dict.member word dictionary


eligibleWord : String -> List String -> Bool
eligibleWord word foundWords =
    not (List.member word foundWords)


markAtIndex : Int -> List Letter -> List Letter
markAtIndex index letters =
    case letters of
        ((Letter ch _) as letter) :: rest ->
            (if index == 0 then
                Letter ch True

             else
                letter
            )
                :: markAtIndex (index - 1) rest

        [] ->
            []


unmarkAtIndex : Int -> List Letter -> List Letter
unmarkAtIndex index letters =
    case letters of
        ((Letter ch _) as letter) :: rest ->
            (if index == 0 then
                Letter ch False

             else
                letter
            )
                :: unmarkAtIndex (index - 1) rest

        [] ->
            []


unmarkAll : List Letter -> List Letter
unmarkAll letters =
    case letters of
        (Letter ch _) :: rest ->
            Letter ch False :: unmarkAll rest

        [] ->
            []


keyEventToCmd : Model -> Decode.Decoder Msg
keyEventToCmd model =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    -- Enter key
                    "Enter" ->
                        Decode.succeed PlayerPressedSubmit

                    -- Backspace key
                    "Backspace" ->
                        Decode.succeed Backspace

                    other ->
                        if String.length other == 1 then
                            other
                                |> String.uncons
                                |> Maybe.map
                                    (\( ch, _ ) ->
                                        case findUnselectedLetter model.game.letters ch of
                                            Just index ->
                                                Decode.succeed (AddLetter ch index)

                                            Nothing ->
                                                Decode.fail "can't type that letter"
                                    )
                                |> Maybe.withDefault
                                    (Decode.fail "can't type that letter")

                        else
                            Decode.fail "length of key /= 1???"
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Wordy"
    , body =
        [ case model.screen of
            Menu ->
                viewMenu model

            Game ->
                viewGame model.game
        ]
    }


viewMenu : Model -> Html Msg
viewMenu _ =
    let
        content : Element Msg
        content =
            Element.column
                [ Element.spacing 40
                ]
                [ UI.largeButton []
                    { label = Element.text "Play"
                    , onPress = Just NewGame
                    }
                    |> Element.el [ Element.centerX ]
                , Element.row []
                    [ Element.paragraph [ Element.paddingXY 32 0 ]
                        [ Element.text "How many words can you find? Words must be at least three letters."
                        ]
                    ]
                ]
    in
    Element.column
        [ Element.centerX
        , Element.centerY
        , Element.spacing 64
        ]
        [ Element.row [ Element.centerX ]
            [ Element.el
                [ Region.heading 1
                , Font.bold
                , Font.size 72
                ]
                (Element.text "Wordy")
            ]
        , content
        ]
        |> Element.layout []


viewIcon : (Phosphor.IconWeight -> Phosphor.IconVariant) -> Element Msg
viewIcon icon =
    icon Phosphor.Regular
        |> Phosphor.withSize 32
        |> Phosphor.withSizeUnit "px"
        |> Phosphor.toHtml []
        |> Element.html


viewGame : GameModel -> Html Msg
viewGame game =
    Element.column
        [ Element.centerX
        , Element.centerY
        , Element.spacing 16
        , Element.height Element.fill
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spaceEvenly
            ]
            [ Element.el
                [ Region.heading 1
                , Font.bold
                , Font.size 20
                ]
                (Element.text "Wordy")
            , [ List.length game.foundWords |> String.fromInt
              , " of "
              , String.fromInt game.totalWords
              , " found"
              ]
                |> String.concat
                |> Element.text
                |> Element.el [ Font.size 20 ]
            ]
        , viewLetters game.letters
        , Element.row
            ([ Element.centerX
             , Font.family [ Font.monospace ]
             , Font.bold
             , Font.size 24
             ]
                ++ (if game.feedback == Idle then
                        []

                    else
                        let
                            ( time, text ) =
                                case game.feedback of
                                    AlreadyFound time_ word ->
                                        ( time_
                                        , "Already found " ++ word
                                        )

                                    Idle ->
                                        ( Time.millisToPosix 0, "" )

                                    InvalidWord time_ _ ->
                                        ( time_
                                        , "Not in word list"
                                        )

                                    WordFound time_ _ ->
                                        ( time_
                                        , "Great!"
                                        )

                                    WordTooShort time_ _ ->
                                        ( time_
                                        , "Too short"
                                        )
                        in
                        [ Element.above <|
                            Element.Keyed.el
                                [ Element.centerX
                                , Element.htmlAttribute
                                    (Attributes.style "pointer-events" "none")
                                ]
                                ( time |> Time.posixToMillis |> String.fromInt
                                , Element.el
                                    [ Background.color UI.pink
                                    , Font.color UI.white
                                    , Font.size 30
                                    , Element.htmlAttribute (Attributes.class "anim-fade-out")
                                    , Element.paddingXY 30 16
                                    ]
                                    (Element.text text)
                                )
                        ]
                   )
            )
            [ Element.el
                [ Element.paddingEach { bottom = 0, left = 0, right = 2, top = 0 }
                ]
                (guessToString game.reverseGuess
                    |> String.toUpper
                    |> Element.text
                )
            , Element.el
                [ Element.htmlAttribute (Attributes.class "anim-cursor-blink")
                , Background.color UI.pink
                , Element.width (Element.px 2)
                , Element.height (Element.px 32)
                , Element.alignTop
                ]
                (Element.text "")
            ]
        , Element.row
            [ Element.spaceEvenly
            , Element.width Element.fill
            ]
            [ UI.button []
                { label = viewIcon Phosphor.shuffle
                , onPress = Just Shuffle
                }
            , Element.row
                [ Element.spacing 16
                ]
                [ UI.button []
                    { label = viewIcon Phosphor.backspace
                    , onPress = Just Backspace
                    }
                , UI.button []
                    { label = viewIcon Phosphor.keyReturn
                    , onPress = Just PlayerPressedSubmit
                    }
                ]
            ]
        , Element.el
            [ Element.scrollbarY
            , Element.height Element.fill
            , Element.width Element.fill
            ]
            (viewFoundWords game.foundWords)
        ]
        |> Element.layout
            [ Element.paddingXY 0 32 ]


viewLetters : List Letter -> Element Msg
viewLetters letters =
    let
        rows : List (List (Element Msg))
        rows =
            letters
                |> List.indexedMap viewLetter
                |> List.groupsOf 3
    in
    -- Wrap each group (3 letters) into a row
    rows
        |> List.map
            (Element.row
                [ Element.spacing 16
                ]
            )
        |> Element.column
            [ Element.spacing 16
            , Element.htmlAttribute
                (Attributes.attribute "unselectable" "on")
            ]


viewLetter : Int -> Letter -> Element Msg
viewLetter index (Letter ch selected) =
    let
        letter : String
        letter =
            ch |> String.fromChar |> String.toUpper
    in
    if selected then
        UI.selectedLetterButton { letter = letter, onPress = Nothing }

    else
        UI.letterButton { letter = letter, onPress = Just (AddLetter ch index) }


viewFoundWords : List String -> Element Msg
viewFoundWords foundWords =
    let
        children : List (Element msg)
        children =
            foundWords
                |> List.map (\w -> Element.el [] (String.toUpper w |> Element.text))
    in
    Element.column
        [ Font.family [ Font.monospace ]
        , Font.size 20
        ]
        children
