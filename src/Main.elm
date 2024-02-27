port module Main exposing (Flags, InnerModel, Model, Msg, PlayingStatus, main)

import Audio exposing (Audio, AudioCmd, AudioData)
import Dict exposing (Dict)
import Duration exposing (Duration)
import Element.WithContext as Element exposing (alignBottom, alignRight, centerX, centerY, el, fill, height, px, width)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Http
import Json.Decode
import Json.Encode
import List.Extra
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
    , playing : PlayingStatus
    , loadedTracks : Dict String Audio.Source
    , mainVolume : Float
    }


type PlayingStatus
    = Stopped
    | Playing String Time.Posix
    | Paused String Duration


type InnerModel
    = LoadingPlaylist
    | LoadedPlaylist (List String)
    | LoadingError Http.Error


type Msg
    = LoadedTranslation (Result Http.Error Translations.I18n)
    | SwitchLanguage Translations.Language
    | UntimedMsg TimedMsg
    | TimedMsg TimedMsg Time.Posix
    | SwitchedLanguage (Result Http.Error (Translations.I18n -> Translations.I18n))
    | LoadedAudio (Result Audio.LoadError ( String, Audio.Source ))
    | Volume Float
    | GotPlaylist (Result Http.Error String)


type TimedMsg
    = Play String
    | Pause
    | Resume


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
    case model.playing of
        Playing name from ->
            Dict.get name model.loadedTracks
                |> Maybe.map
                    (\source ->
                        [ Audio.audio source from ]
                            |> Audio.group
                            |> Audio.scaleVolume model.mainVolume
                    )
                |> Maybe.withDefault Audio.silence

        Paused _ _ ->
            Audio.silence

        Stopped ->
            Audio.silence


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
            , inner = LoadingPlaylist
            , loadedTracks = Dict.empty
            , playing = Stopped
            , mainVolume = 0.5
            }
    in
    ( model
    , Cmd.batch
        [ Translations.loadMain
            (\result ->
                result
                    |> Result.map (\f -> f i18n)
                    |> LoadedTranslation
            )
            i18n
        , Http.get
            { url = "/public/playlist.txt"
            , expect = Http.expectString GotPlaylist
            }
        ]
    , Audio.cmdNone
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
        LoadedTranslation (Err _) ->
            pure model

        LoadedTranslation (Ok i18n) ->
            updateContext { context | i18n = i18n }

        GotPlaylist (Err e) ->
            pure { model | inner = LoadingError e }

        GotPlaylist (Ok playlist) ->
            let
                tracks : List String
                tracks =
                    String.split "\n" playlist
                        |> List.Extra.removeWhen String.isEmpty
            in
            ( { model | inner = LoadedPlaylist tracks }
            , Cmd.none
            , tracks
                |> List.map
                    (\name ->
                        Url.Builder.absolute [ "public", name ] []
                            |> Audio.loadAudio
                                (\result -> LoadedAudio <| Result.map (\source -> ( name, source )) result)
                    )
                |> Audio.cmdBatch
            )

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

        TimedMsg (Play song) now ->
            pure { model | playing = Playing song now }

        TimedMsg Pause now ->
            pure
                { model
                    | playing =
                        case model.playing of
                            Playing song from ->
                                Paused song <| Duration.from from now

                            _ ->
                                model.playing
                }

        TimedMsg Resume now ->
            pure
                { model
                    | playing =
                        case model.playing of
                            Paused song duration ->
                                Playing song <| Duration.subtractFrom now duration

                            _ ->
                                model.playing
                }

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
        LoadingPlaylist ->
            el [ centerX, centerY ] <| text <| \_ -> "Loading..."

        LoadedPlaylist _ ->
            innerView model

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


innerView : Model -> Element Msg
innerView model =
    column [ width fill, height fill ]
        [ menuBar
        , Theme.column
            [ Theme.padding
            , width fill
            , height fill
            ]
            [ Input.slider
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
                    Input.labelAbove [ Font.bold ]
                        (text <| \_ -> "Main volume")
                , min = 0
                , max = 1
                , step = Nothing
                , value = model.mainVolume
                , thumb = Input.defaultThumb
                }
            , case model.playing of
                Stopped ->
                    Element.none

                Playing name _ ->
                    Theme.column []
                        [ Theme.row []
                            [ el [ Font.bold ] <| text Translations.playing
                            , Theme.button []
                                { onPress = Just <| UntimedMsg Pause
                                , label = text Translations.pause
                                }
                            ]
                        , text <| \_ -> name
                        ]

                Paused name _ ->
                    Theme.column []
                        [ Theme.row []
                            [ el [ Font.bold ] <| text Translations.paused
                            , Theme.button []
                                { onPress = Just <| UntimedMsg Resume
                                , label = text Translations.resume
                                }
                            ]
                        , text <| \_ -> name
                        ]
            , Dict.keys model.loadedTracks
                |> List.map
                    (\t ->
                        Theme.button []
                            { label =
                                t
                                    |> String.split " - "
                                    |> List.drop 2
                                    |> String.join " - "
                                    |> Translations.play
                                    |> text
                            , onPress = Just <| UntimedMsg <| Play t
                            }
                    )
                |> (::) (el [ Font.bold ] <| text Translations.loaded)
                |> Theme.wrappedRow [ alignBottom ]
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
