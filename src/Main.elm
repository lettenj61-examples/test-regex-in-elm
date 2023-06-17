module Main exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Regex exposing (Regex)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { regexSource : String
    , regex : Maybe Regex
    , changed : RegexChanged
    , input : String
    }


type RegexChanged
    = GoodRegex
    | BadPattern
    | RegexNotChanged


init : flags -> ( Model, Cmd msg )
init _ =
    ( { regexSource = ""
      , regex = Nothing
      , changed = RegexNotChanged
      , input = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | SetRegexSource String
    | NewInputText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        return model_ =
            ( model_, Cmd.none )
    in
    case msg of
        NoOp ->
            return model

        SetRegexSource newSource ->
            let
                newRegex =
                    Regex.fromString newSource

                newModel =
                    { model
                        | regexSource = newSource
                        , regex = newRegex
                        , changed =
                            if model.regexSource == newSource then
                                RegexNotChanged

                            else
                                case newRegex of
                                    Just _ ->
                                        GoodRegex

                                    Nothing ->
                                        BadPattern
                    }
            in
            return newModel

        NewInputText newInput ->
            let
                newModel =
                    { model
                        | input = newInput
                    }
            in
            return newModel



-- VIEW


bigTitleString : String
bigTitleString =
    "Test Regular Expression in Elm!"


view : Model -> Document Msg
view model =
    { title = bigTitleString
    , body = regexTesterView model
    }


regexTesterView : Model -> List (Html Msg)
regexTesterView model =
    [ h1 [] [ text bigTitleString ]
    , section []
        [ p []
            [ label
                [ style "font-weight" "600"
                ]
                [ text "Pattern" ]
            , br [] []
            , input
                [ name "regex-source"
                , type_ "text"
                , maxlength 80
                , placeholder "Type your regexp PATTERN"
                , Events.on "change"
                    (Decode.map SetRegexSource Events.targetValue)
                ]
                []
            ]
        , p []
            [ text <|
                case model.changed of
                    GoodRegex ->
                        "Valid pattern!"

                    RegexNotChanged ->
                        "Waiting for changes..."

                    BadPattern ->
                        "Boo! The pattern is not a valid regexp!"
            ]
        ]
    , section []
        [ h3 [] [ text "Try your regex here!" ]
        , p []
            [ label
                [ style "font-weight" "600"
                ]
                [ text "Input text" ]
            , br [] []
            , textarea
                [ Events.on "change"
                    (Decode.map NewInputText Events.targetValue)
                ]
                [ text model.input
                ]
            ]
        , regexOutputView model
        ]
    ]


regexOutputView : Model -> Html msg
regexOutputView model =
    let
        preCode str =
            pre []
                [ code []
                    [ text str ]
                ]

        matches =
            case model.regex of
                Just r ->
                    Regex.find r model.input

                Nothing ->
                    []

        matchesJson =
            Encode.list matchToValue matches
                |> Encode.encode 2
    in
    div []
        [ preCode matchesJson
        ]



-- HELPERS


renderMaybeAsArray : (a -> Value) -> Maybe a -> Value
renderMaybeAsArray toValue ma =
    Encode.list toValue
        (case ma of
            Just x ->
                [x]

            Nothing ->
                []
        )


matchToValue : Regex.Match -> Value
matchToValue match =
    Encode.object
        [ ( "match", Encode.string match.match )
        , ( "index", Encode.int match.index )
        , ( "number", Encode.int match.number )
        , ( "submatches"
          , Encode.list
                (renderMaybeAsArray Encode.string)
                match.submatches
          )
        ]
