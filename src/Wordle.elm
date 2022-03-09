module Wordle exposing (main)

import Api
import Browser
import Browser.Events exposing (onKeyDown)
import Dico exposing (dico)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Task
import Time exposing (Month(..), toDay, toMonth, toYear, utc)



-- Model


type alias Grid =
    { words : List Word
    , current : Word
    , wordToFind : Word
    , guessesMax : Int
    , state : GameState
    , debug : String
    , wordsDone : List Word
    , wordLength : Int
    , timeOfTheDay : Maybe Time.Posix
    }


type ErrorType
    = TimeFailed
    | NoMoreWord


type alias Cell =
    ( Char, CellState )


type CellState
    = Found
    | Exist
    | NotExist


type GameState
    = Loading
    | Running
    | GameOver
    | Won
    | WordNotInDico
    | Error ErrorType


type alias Word =
    String


type alias WordValidated =
    List Cell


wordEmpty : Word
wordEmpty =
    ""


guessesMax : Int
guessesMax =
    6



-- TODO : convert wordToFind to a maybe...


initialGrid : Grid
initialGrid =
    { words = []
    , current = wordEmpty
    , wordToFind = ""
    , guessesMax = guessesMax
    , state = Loading
    , debug = ""
    , wordsDone = []
    , wordLength = 5
    , timeOfTheDay = Nothing
    }



-- SUBSCRIPTIONS


subscriptions : Grid -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKey char

        Just ( 'E', "nter" ) ->
            EnterKey

        Just ( 'B', "ackspace" ) ->
            DeleteKey

        _ ->
            ControlKey keyValue



-- INIT


init : Encode.Value -> ( Grid, Cmd Msg )
init initialData =
    ( case Decode.decodeValue decoderInit initialData of
        Ok localData ->
            { initialGrid | wordsDone = localData.wordsDone, words = localData.words }

        Err message ->
            { initialGrid | wordsDone = [], debug = Debug.toString message }
    , getWordOfTheDay
    )



-- KEYBOARD


type Keyboard
    = Key Char
    | Delete String
    | Enter Char


getKeyboard : List Keyboard
getKeyboard =
    [ Key 'A', Key 'Z', Key 'E', Key 'R', Key 'T', Key 'Y', Key 'U', Key 'I', Key 'O', Key 'P', Key 'Q', Key 'S', Key 'D', Key 'F', Key 'G', Key 'H', Key 'J', Key 'K', Key 'L', Key 'M', Enter '⏎', Key 'W', Key 'X', Key 'C', Key 'V', Key 'B', Key 'N', Delete "DEL" ]



-- MAIN


main : Program Encode.Value Grid Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE


type Msg
    = HandleKey Keyboard
    | CharacterKey Char
    | ControlKey String
    | EnterKey
    | DeleteKey
    | GetWordOfTheDay Time.Posix
    | NewGame


update : Msg -> Grid -> ( Grid, Cmd Msg )
update msg grid =
    case msg of
        NewGame ->
            let
                dicoFiltered =
                    dico
                        |> List.filter
                            (\word ->
                                String.length word
                                    == grid.wordLength
                                    && not (List.member word grid.wordsDone)
                            )

                newWord =
                    grid.timeOfTheDay
                        |> Maybe.map
                            (\time ->
                                Random.step (Random.int 0 (List.length dicoFiltered)) (Random.initialSeed (dateToInt time))
                            )
                        |> Maybe.andThen
                            (\( _, seed ) ->
                                getNewRandomWord dicoFiltered seed
                            )

                localData =
                    LocalData grid.wordsDone []
            in
            case newWord of
                Nothing ->
                    ( { grid | debug = Debug.toString dicoFiltered, state = Error TimeFailed }, Cmd.none )

                Just w ->
                    ( { grid | words = [], current = wordEmpty, wordToFind = w, state = Running }, updateLocalStorage localData )

        GetWordOfTheDay time ->
            let
                dicoFiltered =
                    dico
                        |> List.filter
                            (\dicoWord ->
                                String.length dicoWord
                                    == grid.wordLength
                                    && not (List.member dicoWord grid.wordsDone)
                            )

                ( _, seed ) =
                    Random.step (Random.int 0 (List.length dicoFiltered)) (Random.initialSeed (dateToInt time))

                wordToFind =
                    getNewRandomWord dicoFiltered seed
            in
            case wordToFind of
                Nothing ->
                    ( { grid | debug = Debug.toString dicoFiltered, state = Error TimeFailed }, Cmd.none )

                Just w ->
                    ( { grid | wordToFind = w, state = Running, timeOfTheDay = Just time }, Cmd.none )

        EnterKey ->
            handleKey (Enter '⏎') grid

        DeleteKey ->
            handleKey (Delete "DEL") grid

        CharacterKey char ->
            let
                isCharCorrect =
                    Char.isAlpha char

                charUpper =
                    Char.toUpper char

                keyFromChar =
                    if isCharCorrect then
                        Just (Key charUpper)

                    else
                        Nothing
            in
            case keyFromChar of
                Nothing ->
                    ( grid, Cmd.none )

                Just key ->
                    handleKey key grid

        HandleKey key ->
            handleKey key grid

        _ ->
            ( grid, Cmd.none )


dateToInt : Time.Posix -> Int
dateToInt time =
    let
        date =
            String.fromInt (toYear utc time) ++ String.fromInt (monthToNumber (toMonth utc time)) ++ String.fromInt (toDay utc time + 1)
    in
    date
        |> String.toInt
        |> Maybe.withDefault 404


getNewRandomWord : List Word -> Random.Seed -> Maybe Word
getNewRandomWord dico seed =
    let
        ( randomNumber, _ ) =
            Random.step (Random.int 0 (List.length dico)) seed
    in
    dico
        |> List.drop randomNumber
        |> List.head


getWordOfTheDay : Cmd Msg
getWordOfTheDay =
    Task.perform GetWordOfTheDay Time.now


monthToNumber : Month -> Int
monthToNumber month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


handleKey : Keyboard -> Grid -> ( Grid, Cmd Msg )
handleKey key grid =
    let
        cellFilled =
            String.length grid.current

        wordIsFilled =
            cellFilled == String.length grid.wordToFind
    in
    case ( key, wordIsFilled, grid.state ) of
        ( Key _, True, Running ) ->
            ( grid, Cmd.none )

        ( Key c, _, Running ) ->
            ( { grid | current = grid.current ++ String.fromChar c }, Cmd.none )

        ( Delete _, _, WordNotInDico ) ->
            ( { grid | current = String.dropRight 1 grid.current, state = Running }, Cmd.none )

        ( Delete _, _, Running ) ->
            ( { grid | current = String.dropRight 1 grid.current }, Cmd.none )

        ( Enter _, False, Won ) ->
            update NewGame grid

        ( Enter _, False, GameOver ) ->
            update NewGame grid

        ( Enter _, True, Running ) ->
            if wordIsInDico grid.current == False then
                ( { grid | state = WordNotInDico }, Cmd.none )

            else
                let
                    newWords =
                        grid.current :: grid.words

                    wordsDone =
                        grid.wordToFind :: grid.wordsDone

                    wordIsFound =
                        grid.current == grid.wordToFind

                    hasMoreGuessesLeft =
                        newWords
                            |> List.length
                            |> (>) guessesMax

                    localState =
                        LocalData (grid.wordToFind :: grid.wordsDone) newWords
                in
                case ( wordIsFound, hasMoreGuessesLeft ) of
                    ( True, _ ) ->
                        ( { grid | current = wordEmpty, wordsDone = wordsDone, words = newWords, state = Won }, updateLocalStorage localState )

                    ( _, False ) ->
                        ( { grid | current = wordEmpty, wordsDone = wordsDone, words = newWords, state = GameOver }, updateLocalStorage localState )

                    ( _, _ ) ->
                        ( { grid | current = wordEmpty, words = newWords }, updateLocalStorage localState )

        ( _, _, _ ) ->
            ( grid, Cmd.none )


wordIsInDico : Word -> Bool
wordIsInDico word =
    dico
        |> List.member word


validWord : Word -> Word -> WordValidated
validWord wordToFind word =
    let
        wordWithFoundLetters =
            validFoundLetters wordToFind [] word

        lettersNotFound =
            getNotFoundLetters wordToFind wordWithFoundLetters
    in
    validExistLetters lettersNotFound wordWithFoundLetters []


getNotFoundLetters : Word -> WordValidated -> String
getNotFoundLetters wordToFind wordWithFoundLetters =
    wordWithFoundLetters
        |> List.indexedMap
            (\index cell ->
                case cell of
                    ( _, Found ) ->
                        ""

                    _ ->
                        String.left 1 (String.dropLeft index wordToFind)
            )
        |> String.join ""


validFoundLetters : Word -> WordValidated -> Word -> WordValidated
validFoundLetters wordToFind acc wordGuess =
    let
        maybeSplitGuess =
            String.uncons wordGuess

        maybeSplitFind =
            String.uncons wordToFind
    in
    if String.length wordToFind == 0 then
        acc

    else
        case ( maybeSplitFind, maybeSplitGuess ) of
            ( Just ( charFind, nextFind ), Just ( charGuess, nextGuess ) ) ->
                if charFind == charGuess then
                    validFoundLetters nextFind (acc ++ [ ( charGuess, Found ) ]) nextGuess

                else
                    validFoundLetters nextFind (acc ++ [ ( charGuess, NotExist ) ]) nextGuess

            ( _, _ ) ->
                acc


validExistLetters : String -> WordValidated -> WordValidated -> WordValidated
validExistLetters lettersNotFound wordsFoundLetters acc =
    let
        currentCell =
            List.head wordsFoundLetters

        tail =
            List.tail wordsFoundLetters |> Maybe.withDefault []
    in
    case currentCell of
        Nothing ->
            acc

        Just ( char, Found ) ->
            validExistLetters (removeLetter lettersNotFound char) tail (acc ++ [ ( char, Found ) ])

        Just ( char, _ ) ->
            if String.contains (String.fromChar char) lettersNotFound then
                validExistLetters (removeLetter lettersNotFound char) tail (acc ++ [ ( char, Exist ) ])

            else
                validExistLetters lettersNotFound tail (acc ++ [ ( char, NotExist ) ])


removeLetter : String -> Char -> String
removeLetter str charToRemove =
    let
        firstIndex =
            String.indexes (String.fromChar charToRemove) str
                |> List.head
    in
    case firstIndex of
        Just index ->
            String.left index str ++ String.dropLeft (index + 1) str

        Nothing ->
            str


getLettersPlayed : Word -> List Word -> List ( Keyboard, CellState )
getLettersPlayed wordToFind words =
    words
        |> List.map (validWord wordToFind)
        |> List.foldl getLettersPlayedByWord []


getLettersPlayedByWord : WordValidated -> List ( Keyboard, CellState ) -> List ( Keyboard, CellState )
getLettersPlayedByWord cells lstLetters =
    List.foldl
        (\( char, state ) lst ->
            let
                maybeCharInList =
                    findCharInWord lst char
            in
            case maybeCharInList of
                Nothing ->
                    ( Key char, state ) :: lst

                -- Char can be in list as Exist but there can be
                -- Found one coming after so we need to update char
                Just ( _, listState ) ->
                    case ( state, listState ) of
                        ( Found, _ ) ->
                            updateCharInList lst char Found

                        ( Exist, NotExist ) ->
                            updateCharInList lst char Exist

                        _ ->
                            lst
        )
        lstLetters
        cells


updateCharInList : List ( Keyboard, CellState ) -> Char -> CellState -> List ( Keyboard, CellState )
updateCharInList lst charToUpdate newState =
    lst
        |> List.foldl
            (\( key, oldState ) acc ->
                case key of
                    Key char ->
                        if char == charToUpdate then
                            ( key, newState ) :: acc

                        else
                            ( key, oldState ) :: acc

                    _ ->
                        ( key, oldState ) :: acc
            )
            []


findCharInWord : List ( Keyboard, CellState ) -> Char -> Maybe ( Keyboard, CellState )
findCharInWord lst val =
    List.foldl
        (\cell acc ->
            case ( acc, cell ) of
                ( Just value, _ ) ->
                    Just value

                ( Nothing, ( Key char, state ) ) ->
                    if char == val then
                        Just ( Key char, state )

                    else
                        Nothing

                _ ->
                    Nothing
        )
        Nothing
        lst



-- View


view : Grid -> Html Msg
view grid =
    main_ [ class "container" ]
        [ div [ class "row" ]
            [ h1 [ class "col-6 offset-3 text-center" ] [ text "Wordle" ]
            , case grid.state of
                Won ->
                    buttonPlayAgain

                GameOver ->
                    buttonPlayAgain

                _ ->
                    text ""
            , case grid.state of
                Error typeError ->
                    case typeError of
                        _ ->
                            div [ class "col-6 offset-3 text-center" ] [ text "Oups! Une erreur s'est produite!" ]

                Won ->
                    div [ class "col-6 offset-3 text-center" ] [ text "You won !" ]

                GameOver ->
                    div [ class "col-6 offset-3 text-center" ] [ text "You lost !" ]

                Running ->
                    text ""

                WordNotInDico ->
                    div [ class "col-6 offset-3 text-center" ]
                        [ text "Word not found" ]

                Loading ->
                    div [ class "row justify-content-center", style "min-height" "200px" ]
                        [ div [ class "spinner-border" ]
                            [ span [ class "visually-hidden" ] [ text "Loading..." ]
                            ]
                        ]
            , case grid.state of
                Loading ->
                    text ""

                Error _ ->
                    text ""

                _ ->
                    div []
                        [ div [] (grid.words |> List.reverse |> List.map (validWord grid.wordToFind) |> List.map viewWord)
                        , div [] [ viewCurrent (grid.current |> String.padRight (String.length grid.wordToFind) ' ') ]
                        , div [] (viewEmptyLines grid)
                        ]
            , div [ class "row" ] (List.map (showKey grid) getKeyboard)
            ]
        ]


buttonPlayAgain : Html Msg
buttonPlayAgain =
    div [ class "btn btn-large btn-primary", onClick NewGame ] [ text "Play again !" ]


viewWord : WordValidated -> Html Msg
viewWord word =
    div [ class "row offset-1", style "height" "30px" ] (word |> List.map viewCell)


viewCurrent : Word -> Html Msg
viewCurrent word =
    div [ class "row offset-1", style "height" "30px" ]
        (word
            |> String.toList
            |> List.map
                (\char ->
                    div [ class "col-2", style "border" "1px solid black" ] [ text (String.fromChar char) ]
                )
        )


viewCell : Cell -> Html Msg
viewCell ( c, state ) =
    div [ class ("col-2 " ++ stateToString state), style "border" "1px solid black" ] [ text (String.fromChar c) ]


stateToString : CellState -> String
stateToString state =
    case state of
        Found ->
            "bg-success"

        Exist ->
            "bg-info"

        NotExist ->
            ""


getKeyboardChar : Keyboard -> Maybe Char
getKeyboardChar key =
    case key of
        Key char ->
            Just char

        _ ->
            Nothing


showKey : Grid -> Keyboard -> Html Msg
showKey grid key =
    let
        lettersPlayed =
            getLettersPlayed grid.wordToFind grid.words

        foundLetterToClass : String
        foundLetterToClass =
            key
                |> getKeyboardChar
                |> Maybe.andThen (findCharInWord lettersPlayed)
                |> Maybe.map
                    (\( _, state ) ->
                        case state of
                            NotExist ->
                                "bg-dark text-white"

                            Found ->
                                "bg-success"

                            Exist ->
                                "bg-info"
                    )
                |> Maybe.withDefault ""

        baseClasses =
            [ "text-center", foundLetterToClass ]

        offsetClasses =
            "offset-1" :: baseClasses

        classesToString : List String -> String
        classesToString classes =
            String.join " " classes
    in
    case key of
        Key 'A' ->
            div [ class (classesToString ("col-1" :: offsetClasses)), onClick (HandleKey key) ] [ text "A" ]

        Key 'Q' ->
            div [ class (classesToString ("col-1" :: offsetClasses)), onClick (HandleKey key) ] [ text "Q" ]

        Key c ->
            div [ class (classesToString ("col-1" :: baseClasses)), onClick (HandleKey key) ] [ text (String.fromChar c) ]

        Delete _ ->
            div [ class (classesToString ("col-2" :: baseClasses)), onClick (HandleKey key) ] [ text "˿" ]

        Enter _ ->
            div [ class (classesToString ("col-2" :: offsetClasses)), onClick (HandleKey key) ] [ text "⏎" ]


viewEmptyLines : Grid -> List (Html Msg)
viewEmptyLines grid =
    emptyLines grid
        |> List.map viewCurrent


emptyLines : Grid -> List Word
emptyLines grid =
    let
        tentativeRestante =
            grid.guessesMax - (List.length grid.words + 2)

        wordEmptySpaces =
            List.range 0 (String.length grid.wordToFind - 1)
                |> List.map (always " ")
                |> String.join ""
    in
    List.range 0 tentativeRestante
        |> List.map
            (\_ -> wordEmptySpaces)



-- API


updateLocalStorage : LocalData -> Cmd msg
updateLocalStorage localData =
    Encode.object
        [ ( "wordsDone", Encode.list Encode.string localData.wordsDone )
        , ( "words", Encode.list Encode.string localData.words )
        ]
        |> Encode.encode 0
        |> Api.updateStorage


type alias LocalData =
    { wordsDone : List String
    , words : List String
    }


decoderInit : Decode.Decoder LocalData
decoderInit =
    Decode.map2 LocalData wordsDoneDecoder wordsDecoder


wordsDoneDecoder : Decode.Decoder (List String)
wordsDoneDecoder =
    Decode.field "wordsDone" (Decode.list Decode.string)


wordsDecoder : Decode.Decoder (List String)
wordsDecoder =
    Decode.field "words" (Decode.list Decode.string)
