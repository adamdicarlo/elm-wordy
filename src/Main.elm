module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Dict
import Dictionary
    exposing
        ( Dictionary
        , DictionaryResponse
        , dictionaryFromResponse
        )
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Http
import Json.Decode as Decode
import List.Extra as List
import Phosphor
import Process
import Random exposing (Generator)
import RemoteData exposing (WebData)
import Task
import Tuple
import UI


main : Program () Model Msg
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
                [ onKeyDown <| keyEventToCmd model
                ]

        _ ->
            Sub.none


type Screen
    = Menu
    | Game


type Letter
    = Letter Char Bool


type Feedback
    = AlreadyFound String
    | Idle
    | InvalidWord String
    | WordFound String


type alias GameModel =
    { dictionary : WebData Dictionary
    , feedback : Feedback
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
      -- User action: Submit the guess
    | ClearFeedback
    | SubmitGuess Dictionary
      -- User action: Shuffle the board
    | Shuffle
      -- User action: Start a new game
    | NewGame
      -- AJAX response for dictionary (uses RemoteData package)
    | DictionaryResponse (WebData DictionaryResponse)
      -- RNG response: Numbers used for shuffling letters
    | ShuffleOrdering (List Int)
      -- RNG response: Numbers used for picking a 9-letter word, then shuffling it, when
      -- starting a new game
    | NewGameNumbers ( Int, List Int )
    | NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screen = Menu
      , game =
            { dictionary = RemoteData.Loading
            , feedback = Idle
            , foundWords = []
            , letters = []
            , reverseGuess = []
            , totalWords = 0
            }
      }
    , getDictionary
    )


fallbackWord : String
fallbackWord =
    "flowering"


getDictionary : Cmd Msg
getDictionary =
    Http.get "dictionary.json" Dictionary.decode
        |> RemoteData.sendRequest
        |> Cmd.map DictionaryResponse


getWords : WebData Dictionary -> Dictionary
getWords dictionary =
    let
        fallback : Dictionary
        fallback =
            Dict.singleton fallbackWord ()
    in
    RemoteData.toMaybe dictionary
        |> Maybe.withDefault fallback


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


totalWords : WebData Dictionary -> List Letter -> Int
totalWords dictionary letters =
    let
        charList =
            lettersToCharList letters

        predicate word _ =
            isWordInBoard word charList
    in
    getWords dictionary
        |> Dict.filter predicate
        |> Dict.size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ game } as model) =
    case msg of
        DictionaryResponse response ->
            case response of
                RemoteData.Success data ->
                    ( { model
                        | game =
                            { game
                                | dictionary =
                                    RemoteData.Success (dictionaryFromResponse data.dictionary)
                            }
                      }
                    , Cmd.none
                    )

                RemoteData.Failure err ->
                    ( { model
                        | game =
                            { game
                                | dictionary = RemoteData.Failure err
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

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

        ClearFeedback ->
            ( { model
                | game =
                    { game | feedback = Idle }
              }
            , Cmd.none
            )

        SubmitGuess dictionary ->
            let
                guess =
                    guessToString game.reverseGuess

                isValid =
                    validWord guess dictionary

                isAlreadyFound =
                    not <| eligibleWord guess game.foundWords

                ( newFoundWords, feedback ) =
                    if isValid then
                        if isAlreadyFound then
                            ( game.foundWords, AlreadyFound guess )

                        else
                            ( guess :: game.foundWords, WordFound guess )

                    else
                        ( game.foundWords, InvalidWord guess )
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
            , Process.sleep 1000
                |> Task.perform (\_ -> ClearFeedback)
            )

        Shuffle ->
            let
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
                generator =
                    Random.pair (selectWordGenerator game.dictionary) (shuffleWordGenerator 9)
            in
            ( model
            , Random.generate NewGameNumbers generator
            )

        NewGameNumbers ( wordIndex, shuffleNumbers ) ->
            let
                letters =
                    nineLetterWords game.dictionary
                        |> Dict.keys
                        |> List.drop wordIndex
                        |> List.head
                        |> Maybe.withDefault fallbackWord
                        |> stringToLetterList
                        |> shuffle shuffleNumbers
            in
            ( { model
                | screen = Game
                , game =
                    { game
                        | letters = letters
                        , totalWords = totalWords game.dictionary letters
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


nineLetterWords : WebData Dictionary -> Dictionary
nineLetterWords dictionary =
    let
        predicate : String -> () -> Bool
        predicate word _ =
            String.length word == 9
    in
    getWords dictionary
        |> Dict.filter predicate


selectWordGenerator : WebData Dictionary -> Generator Int
selectWordGenerator dictionary =
    let
        nineLetterWordCount =
            Dict.size (nineLetterWords dictionary)
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

        sorted =
            List.sortBy Tuple.first zipped
    in
    List.unzip sorted |> Tuple.second


findUnselectedLetter : List Letter -> Char -> Maybe Int
findUnselectedLetter letters sought =
    let
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
                        model.game.dictionary
                            |> RemoteData.toMaybe
                            |> Maybe.map (\dict -> Decode.succeed (SubmitGuess dict))
                            |> Maybe.withDefault (Decode.fail "SubmitGuess without dictionary")

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


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadPayload desc _ ->
            "Bad payload: " ++ desc

        Http.BadStatus { status } ->
            "Bad status: " ++ String.fromInt status.code ++ " " ++ status.message

        Http.BadUrl _ ->
            "Bad URL"

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error (details unknown)"


viewMenu : Model -> Html Msg
viewMenu model =
    let
        content =
            case model.game.dictionary of
                RemoteData.NotAsked ->
                    Element.paragraph [] [ Element.text "Starting" ]

                RemoteData.Loading ->
                    Element.paragraph [] [ Element.text "Loading..." ]

                RemoteData.Failure err ->
                    Element.paragraph [] [ Element.text ("Error: " ++ httpErrorToString err) ]

                RemoteData.Success _ ->
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
        , Element.el
            [ Element.centerX
            , Font.family [ Font.monospace ]
            , Font.bold
            , Font.size 24
            , Element.above
                (Element.el
                    [ Background.color UI.white
                    , Font.size 32
                    , Element.paddingXY 30 16
                    , Element.centerX
                    , Element.alpha
                        (if game.feedback == Idle then
                            0.0

                         else
                            0.9
                        )
                    ]
                    (Element.text <|
                        case game.feedback of
                            AlreadyFound word ->
                                "Already found"

                            Idle ->
                                ""

                            InvalidWord word ->
                                "Bad word"

                            WordFound word ->
                                "Great!"
                    )
                )
            ]
            (guessToString game.reverseGuess
                |> String.toUpper
                -- Hack: Always render at least a space, in order to reserve vertical space.
                |> String.append " "
                |> Element.text
            )
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
                    , onPress =
                        game.dictionary
                            |> RemoteData.toMaybe
                            |> Maybe.map
                                SubmitGuess
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
                (Html.Attributes.attribute "unselectable" "on")
            ]


viewLetter : Int -> Letter -> Element Msg
viewLetter index (Letter ch selected) =
    let
        label =
            ch |> String.fromChar |> String.toUpper |> Element.text
    in
    if selected then
        UI.selectedLetterButton [] { label = label, onPress = Nothing }

    else
        UI.letterButton [] { label = label, onPress = Just (AddLetter ch index) }


viewFoundWords : List String -> Element Msg
viewFoundWords foundWords =
    let
        children =
            foundWords
                |> List.map (\w -> Element.el [] (String.toUpper w |> Element.text))
    in
    Element.column
        [ Font.family [ Font.monospace ]
        , Font.size 20
        ]
        children
