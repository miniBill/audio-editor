port module Main exposing (Flags, InnerModel, Model, Msg, main)

import Audio exposing (Audio, AudioCmd, AudioData)
import Dict exposing (Dict)
import Element.WithContext as Element exposing (alignRight, centerX, centerY, el, fill, height, px, width)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Input as Input
import Http
import Json.Decode
import Json.Encode
import Task
import Theme exposing (Context, Element, column, text)
import Time
import Translations
import Url.Builder


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


type alias Flags =
    { language : String }


type alias Model =
    { context : Context
    , inner : InnerModel
    , playingFrom : Maybe Time.Posix
    , loadedTracks : Dict String Audio.Source
    , mainVolume : Float
    }


type InnerModel
    = Loading
    | Loaded LoadedModel
    | LoadingError Http.Error


type alias LoadedModel =
    {}


type Msg
    = LoadedTranslation (Result Http.Error Translations.I18n)
    | SwitchLanguage Translations.Language
    | UntimedMsg TimedMsg
    | TimedMsg TimedMsg Time.Posix
    | SwitchedLanguage (Result Http.Error (Translations.I18n -> Translations.I18n))
    | LoadedAudio (Result Audio.LoadError ( String, Audio.Source ))
    | Volume Float


type TimedMsg
    = Play


main : Program Flags (Audio.Model Msg Model) (Audio.Msg Msg)
main =
    Audio.elementWithAudio
        { init = init
        , view =
            \audioData model ->
                Element.layout model.context
                    [ Theme.fontSizes.normal
                    , Background.color Theme.colors.background
                    , width fill
                    , height fill
                    ]
                    (view audioData model)
        , update = update
        , subscriptions = subscriptions
        , audio = audio
        , audioPort = audioPort
        }


audioPort :
    { toJS : Json.Encode.Value -> Cmd msg
    , fromJS : (Json.Decode.Value -> msg) -> Sub msg
    }
audioPort =
    { toJS = audioPortToJS
    , fromJS = audioPortFromJS
    }


audio : AudioData -> Model -> Audio
audio _ model =
    case
        Maybe.map2 Tuple.pair
            (Dict.get "First.mp3" model.loadedTracks)
            model.playingFrom
    of
        Nothing ->
            Audio.silence

        Just ( first, playingFrom ) ->
            [ Audio.audio first playingFrom ]
                |> Audio.group
                |> Audio.scaleVolume model.mainVolume


init : Flags -> ( Model, Cmd Msg, AudioCmd Msg )
init flags =
    let
        i18n : Translations.I18n
        i18n =
            Translations.init
                { lang =
                    flags.language
                        |> Translations.languageFromString
                        |> Maybe.withDefault Translations.En
                , path = "/dist/i18n"
                }

        model : Model
        model =
            { context = { i18n = i18n }
            , inner = Loading
            , loadedTracks = Dict.empty
            , playingFrom = Nothing
            , mainVolume = 0.5
            }
    in
    ( model
    , Translations.loadMain
        (\result ->
            result
                |> Result.map (\f -> f i18n)
                |> LoadedTranslation
        )
        i18n
    , [ "Calm.mp3", "First.mp3" ]
        |> List.map
            (\name ->
                Url.Builder.absolute [ "public", name ] []
                    |> Audio.loadAudio
                        (\result -> LoadedAudio <| Result.map (\source -> ( name, source )) result)
            )
        |> Audio.cmdBatch
    )


update : AudioData -> Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg )
update _ msg ({ context } as model) =
    let
        updateContext : Context -> ( Model, Cmd msg, AudioCmd msg )
        updateContext newContext =
            pure { model | context = newContext }

        pure : a -> ( a, Cmd msg, AudioCmd msg )
        pure x =
            ( x, Cmd.none, Audio.cmdNone )
    in
    case msg of
        LoadedTranslation (Err e) ->
            pure { model | inner = LoadingError e }

        LoadedTranslation (Ok i18n) ->
            pure
                { model
                    | context = { context | i18n = i18n }
                    , inner = Loaded {}
                }

        SwitchedLanguage (Err _) ->
            pure model

        SwitchedLanguage (Ok updater) ->
            updateContext { context | i18n = updater context.i18n }

        SwitchLanguage language ->
            let
                ( i18n, cmd ) =
                    Translations.switchLanguage language SwitchedLanguage context.i18n
            in
            ( { model | context = { context | i18n = i18n } }
            , cmd
            , Audio.cmdNone
            )

        UntimedMsg inner ->
            ( model
            , Task.perform (TimedMsg inner) Time.now
            , Audio.cmdNone
            )

        TimedMsg Play now ->
            pure { model | playingFrom = Just now }

        LoadedAudio (Err e) ->
            let
                _ =
                    Debug.log "Error loading audio" e
            in
            pure model

        LoadedAudio (Ok ( name, source )) ->
            pure { model | loadedTracks = Dict.insert name source model.loadedTracks }

        Volume volume ->
            pure { model | mainVolume = volume }


view : AudioData -> Model -> Element Msg
view _ model =
    case model.inner of
        Loading ->
            el [ centerX, centerY ] <| text <| \_ -> "Loading..."

        Loaded loadedModel ->
            innerView model loadedModel

        LoadingError e ->
            column []
                [ text <| \_ -> errorToString e
                ]


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.Timeout ->
            "Connection timed out, try refreshing the page."

        Http.NetworkError ->
            "Error connecting to the server, try refreshing the page."

        _ ->
            "Something went badly, try refreshing the page."


innerView : Model -> LoadedModel -> Element Msg
innerView model _ =
    column [ width fill, height fill ]
        [ menuBar
        , Theme.column [ Theme.padding ]
            [ Theme.button []
                { label = text Translations.play
                , onPress = Just <| UntimedMsg Play
                }
            , ("Loaded" :: Dict.keys model.loadedTracks)
                |> List.map (\t -> text <| \_ -> t)
                |> Theme.column []
            , Input.slider
                [ height (px 30)
                , width fill

                -- Here is where we're creating/styling the "track"
                , Element.behindContent
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 2)
                        , Element.centerY
                        , Background.color Theme.colors.gray
                        , Border.rounded 2
                        ]
                        Element.none
                    )
                ]
                { onChange = Volume
                , label =
                    Input.labelAbove []
                        (text <| \_ -> "Main volume")
                , min = 0
                , max = 1
                , step = Nothing
                , value = model.mainVolume
                , thumb = Input.defaultThumb
                }
            ]
        ]


menuBar : Element Msg
menuBar =
    Theme.row
        [ Theme.padding
        , width fill
        , Background.color Theme.colors.gray
        ]
        [ el [ alignRight ] languagePicker ]


languagePicker : Element Msg
languagePicker =
    Element.with .i18n <|
        \i18n ->
            let
                current : Translations.Language
                current =
                    Translations.currentLanguage i18n

                languageToOption : Translations.Language -> Input.Option { i18n : Translations.I18n } Translations.Language Msg
                languageToOption language =
                    Input.option language <| text <| \_ -> Translations.languageToString language
            in
            Input.radioRow [ Theme.spacing ]
                { label = Input.labelHidden "Language"
                , onChange = SwitchLanguage
                , options = List.map languageToOption Translations.languages
                , selected = Just current
                }


subscriptions : AudioData -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
