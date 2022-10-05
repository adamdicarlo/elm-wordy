module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Char
import Dict
import Dictionary
    exposing
        ( Dictionary
        , DictionaryResponse
        , decodeDictionary
        , dictionaryFromResponse
        )
import Element exposing (Element)
import Element.Events as Events
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Http
import Json.Decode as Decode
import List.Extra as List
import Random exposing (Generator)
import RemoteData exposing (WebData)
import Tuple exposing (first, second)
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


type alias GameModel =
    { dictionary : WebData Dictionary
    , letters : List Letter
    , reverseGuess : List ( Char, Int )
    , foundWords : List String
    , totalWords : Int
    }


type alias Model =
    { screen : Screen
    , game : GameModel
    }


type Msg
    = -- User action: Adds the letter (Char) from a position (Int) to the guess
      AddLetter Char Int
      -- User action: Erase last letter of guess
    | Backspace
      -- User action: Submit the guess
    | SubmitGuess
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
            , letters = []
            , reverseGuess = []
            , foundWords = []
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
    Http.get "/dictionary.json" decodeDictionary
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

        SubmitGuess ->
            let
                guess =
                    guessToString game.reverseGuess

                newFoundWords =
                    case game.dictionary of
                        RemoteData.Success dictionary ->
                            if validWord guess dictionary && eligibleWord guess game.foundWords then
                                guess :: game.foundWords

                            else
                                game.foundWords

                        _ ->
                            game.foundWords
            in
            ( { model
                | game =
                    { game
                        | reverseGuess = []
                        , letters = unmarkAll game.letters
                        , foundWords = newFoundWords
                    }
              }
            , Cmd.none
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
            List.sortBy first zipped
    in
    List.unzip sorted |> second


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
        |> Decode.map
            (\key ->
                case key of
                    -- Enter key
                    "Enter" ->
                        SubmitGuess

                    -- Backspace key
                    "Backspace" ->
                        Backspace

                    other ->
                        if String.length other == 1 then
                            other
                                |> String.uncons
                                |> Maybe.map
                                    (\( ch, _ ) ->
                                        case findUnselectedLetter model.game.letters ch of
                                            Just index ->
                                                AddLetter ch index

                                            Nothing ->
                                                NoOp
                                    )
                                |> Maybe.withDefault NoOp

                        else
                            NoOp
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
                    UI.largeButton []
                        { label = Element.text "New Game"
                        , onPress = Just NewGame
                        }
    in
    Element.column []
        [ Element.el [ Region.heading 1 ] (Element.text "Wordy")
        , content
        ]
        |> Element.layout []


htmlClass : String -> Element.Attribute Msg
htmlClass =
    Element.htmlAttribute << Html.Attributes.class


viewGame : GameModel -> Html Msg
viewGame game =
    Element.column []
        [ Element.el [ Region.heading 1 ] (Element.text "Wordy")
        , viewLetters game.letters
        , Element.el [ htmlClass "guess" ]
            (guessToString game.reverseGuess
                |> String.toUpper
                |> Element.text
            )
        , Element.row [ htmlClass "primaryButtons" ]
            [ UI.button [] { label = Element.text "Backspace", onPress = Just Backspace }
            , UI.button [] { label = Element.text "Shuffle", onPress = Just Shuffle }
            , UI.button [] { label = Element.text "Submit word", onPress = Just SubmitGuess }
            ]
        , [ List.length game.foundWords |> String.fromInt, " found" ]
            |> String.concat
            |> Element.text
        , Element.text (String.fromInt game.totalWords ++ " total")
        , viewFoundWords game.foundWords
        ]
        |> Element.layout []


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
        |> List.map (Element.row [ htmlClass "letterRow" ])
        |> Element.column
            [ htmlClass "letterGrid"
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
    Element.column [ htmlClass "foundWords" ] children
