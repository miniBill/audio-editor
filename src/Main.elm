port module Main exposing (Flags, InnerModel, Model, Msg, PlayingStatus, Timed, main)

import Audio exposing (Audio, AudioCmd, AudioData)
import Browser.Events
import Duration exposing (Duration)
import Float.Extra
import Html exposing (Html)
import Http
import Json.Decode
import Json.Encode
import List.Extra
import MyUi as Ui exposing (Element, alignBottom, alignRight, centerX, centerY, column, el, fill, height, padding, px, shrink, spacing, width)
import MyUi.Events as Events
import MyUi.Font as Font
import MyUi.Input as Input exposing (Label)
import MyUi.Lazy as Lazy
import MyUi.Table as Table
import Quantity exposing (Quantity)
import Round
import Task
import Theme exposing (text, textInvariant)
import Time
import Translations
import Types exposing (AudioSummary, Context, Track)
import Ui as VanillaUi
import Ui.Font
import Url.Builder
import View.Waveform


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


port getAudioSummary : { url : String, samples : Int } -> Cmd msg


port gotAudioSummary : (Json.Decode.Value -> msg) -> Sub msg


type alias Flags =
    { language : String
    , now : Int
    , hasAudio : Bool
    , sampleRate : Int
    , width : Int
    , height : Int
    }


type alias Model =
    { context : Context
    , inner : InnerModel
    , playing : PlayingStatus
    , tracks : List Track
    , mainVolume : Float
    , now : Time.Posix
    , sampleRate : Int
    , width : Int
    , height : Int
    }


type PlayingStatus
    = Playing Time.Posix
    | Paused Duration


type InnerModel
    = LoadingPlaylist
    | LoadedPlaylist (List String)
    | LoadingError Http.Error


type Msg
    = LoadedTranslation (Result Http.Error Translations.I18n)
    | SwitchLanguage Translations.Language
    | SwitchedLanguage (Result Http.Error (Translations.I18n -> Translations.I18n))
    | LoadedAudio (Result Audio.LoadError { name : String, url : String, source : Audio.Source })
    | Volume Float
    | GotPlaylist (Result Http.Error String)
    | Tick
    | GotAudioSummary Json.Decode.Value
    | Resize Int Int
    | WaveformMsg View.Waveform.Msg
    | AddTrack String
    | Play
    | PauseResume
    | Stop
    | RemoveTrack String


type Timed msg
    = Untimed msg
    | Timed msg Time.Posix


main : Program Flags (Audio.Model (Timed Msg) (Maybe Model)) (Audio.Msg (Timed Msg))
main =
    Audio.elementWithAudio
        { init = \flags -> mapTriple (init flags)
        , view =
            \audioData model ->
                maybe noAudioError outerView audioData model
                    |> Html.map Untimed
        , update =
            \audioData msg maybeModel ->
                case maybeModel of
                    Nothing ->
                        ( maybeModel, Cmd.none, Audio.cmdNone )

                    Just model ->
                        case msg of
                            Timed innerMsg now ->
                                let
                                    ( newModel, cmd, audioCmd ) =
                                        update audioData now innerMsg model
                                in
                                mapTriple ( Just newModel, cmd, audioCmd )

                            Untimed innerMsg ->
                                ( maybeModel
                                , Task.perform (Timed innerMsg) Time.now
                                , Audio.cmdNone
                                )
        , subscriptions = maybe Sub.none subscriptions
        , audio = maybe Audio.silence audio
        , audioPort = audioPort
        }


mapTriple :
    ( Maybe Model, Cmd Msg, AudioCmd Msg )
    -> ( Maybe Model, Cmd (Timed Msg), AudioCmd (Timed Msg) )
mapTriple ( model, cmd, audioCmd ) =
    ( model, Cmd.map Untimed cmd, Audio.cmdMap Untimed audioCmd )


noAudioError : Html Msg
noAudioError =
    VanillaUi.layout
        [ VanillaUi.centerX
        , VanillaUi.centerY
        , Ui.Font.size 30
        ]
        (VanillaUi.text "Audio not supported")


outerView : AudioData -> Model -> Html Msg
outerView audioData model =
    Ui.layout model.context
        [ Theme.fontSizes.normal
        , Ui.background Theme.colors.background
        , height fill
        ]
        (view audioData model)


maybe : result -> (AudioData -> Model -> result) -> AudioData -> Maybe Model -> result
maybe default f audioData maybeModel =
    case maybeModel of
        Nothing ->
            default

        Just model ->
            f audioData model


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
        Playing from ->
            model.tracks
                |> List.map
                    (\{ source, offset } ->
                        Audio.audio source (Duration.addTo from offset)
                    )
                |> Audio.group
                |> Audio.scaleVolume model.mainVolume

        Paused _ ->
            Audio.silence


init : Flags -> ( Maybe Model, Cmd Msg, AudioCmd Msg )
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

        model : Maybe Model
        model =
            if flags.hasAudio then
                { context = { i18n = i18n }
                , inner = LoadingPlaylist
                , tracks = []
                , playing = Paused Quantity.zero
                , mainVolume = 0.5
                , now = Time.millisToPosix flags.now
                , sampleRate = flags.sampleRate
                , width = flags.width
                , height = flags.height
                }
                    |> Just

            else
                Nothing
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
            , expect = Http.expectString (\playlist -> GotPlaylist playlist)
            }
        ]
    , Audio.cmdNone
    )


update : AudioData -> Time.Posix -> Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg )
update audioData now msg ({ context } as model) =
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
            pure { model | inner = LoadedPlaylist tracks }

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

        Play ->
            let
                newModel : Model
                newModel =
                    { model | playing = Playing now }
            in
            ( newModel
            , Cmd.none
            , Audio.cmdNone
            )

        PauseResume ->
            pure
                { model
                    | playing =
                        case model.playing of
                            Playing from ->
                                Paused <| Duration.from from now

                            Paused duration ->
                                Playing <| Duration.subtractFrom now duration
                }

        Stop ->
            pure { model | playing = Paused Quantity.zero }

        LoadedAudio (Err e) ->
            let
                _ =
                    Debug.log "Error loading audio" e
            in
            pure model

        LoadedAudio (Ok { name, url, source }) ->
            let
                newTrack : Track
                newTrack =
                    { name = name
                    , url = url
                    , source = source
                    , summary = Nothing
                    , offset = Quantity.zero
                    , duration = Audio.length audioData source
                    }

                newModel : Model
                newModel =
                    { model | tracks = model.tracks ++ [ newTrack ] }
            in
            ( newModel
            , getMissingAudioSummaries newModel
            , Audio.cmdNone
            )

        Volume volume ->
            pure { model | mainVolume = volume }

        Tick ->
            { model | now = now }
                |> stopOnSongEnd audioData
                |> pure

        GotAudioSummary value ->
            let
                decoder : Json.Decode.Decoder { url : String, summary : AudioSummary }
                decoder =
                    Json.Decode.map2
                        (\url summary -> { url = url, summary = summary })
                        (Json.Decode.field "url" Json.Decode.string)
                        (Json.Decode.field "data" <|
                            Json.Decode.list <|
                                Json.Decode.list <|
                                    Json.Decode.map3 (\min rms max -> ( min, rms, max ))
                                        (Json.Decode.field "0" Json.Decode.float)
                                        (Json.Decode.field "1" Json.Decode.float)
                                        (Json.Decode.field "2" Json.Decode.float)
                        )
            in
            case Json.Decode.decodeValue decoder value of
                Err e ->
                    let
                        _ =
                            Debug.log "Error loading raw data" e
                    in
                    pure model

                Ok { url, summary } ->
                    pure
                        { model
                            | tracks =
                                List.map
                                    (\track ->
                                        if track.url == url then
                                            { track | summary = Just summary }

                                        else
                                            track
                                    )
                                    model.tracks
                        }

        Resize width height ->
            let
                newModel : Model
                newModel =
                    { model
                        | width = width
                        , height = height
                    }
            in
            ( newModel
            , getMissingAudioSummaries { newModel | tracks = List.map (\track -> { track | summary = Nothing }) model.tracks }
            , Audio.cmdNone
            )

        WaveformMsg waveformMsg ->
            case waveformMsg of
                View.Waveform.Up at ->
                    pure
                        { model
                            | playing =
                                case model.playing of
                                    Playing _ ->
                                        Playing (Duration.subtractFrom model.now at)

                                    Paused _ ->
                                        Paused at
                        }

                View.Waveform.Down _ ->
                    pure model

                View.Waveform.Move _ ->
                    pure model

        AddTrack name ->
            ( model
            , Cmd.none
            , loadAudio name <| songNameToUrl name
            )

        RemoveTrack name ->
            pure { model | tracks = List.Extra.removeWhen (\track -> track.name == name) model.tracks }


totalLength : AudioData -> Model -> Maybe (Quantity Float Duration.Seconds)
totalLength audioData model =
    model.tracks
        |> List.map
            (\{ source, offset } ->
                Audio.length audioData source
                    |> Quantity.plus offset
            )
        |> Quantity.maximum


loadAudio : String -> String -> AudioCmd Msg
loadAudio name url =
    Audio.loadAudio
        (\result ->
            LoadedAudio <|
                Result.map
                    (\source ->
                        { name = name
                        , url = url
                        , source = source
                        }
                    )
                    result
        )
        url


getMissingAudioSummaries : Model -> Cmd Msg
getMissingAudioSummaries model =
    model.tracks
        |> List.filterMap
            (\{ url, summary } ->
                case summary of
                    Nothing ->
                        Just <| getAudioSummary { url = url, samples = waveviewWidth model }

                    Just _ ->
                        Nothing
            )
        |> Cmd.batch


songNameToUrl : String -> String
songNameToUrl name =
    Url.Builder.absolute [ "public", name ] []


stopOnSongEnd : AudioData -> Model -> Model
stopOnSongEnd audioData model =
    case model.playing of
        Playing from ->
            case totalLength audioData model of
                Just duration ->
                    if
                        Duration.from from model.now
                            |> Quantity.greaterThanOrEqualTo duration
                    then
                        { model | playing = Paused Quantity.zero }

                    else
                        model

                Nothing ->
                    { model | playing = Paused Quantity.zero }

        Paused _ ->
            model


view : AudioData -> Model -> Element Msg
view audioData model =
    case model.inner of
        LoadingPlaylist ->
            el [ centerX, centerY ] <| textInvariant "Loading..."

        LoadedPlaylist playlist ->
            innerView audioData model playlist

        LoadingError e ->
            textInvariant <| errorToString e


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.Timeout ->
            "Connection timed out, try refreshing the page."

        Http.NetworkError ->
            "Error connecting to the server, try refreshing the page."

        _ ->
            "Something went badly, try refreshing the page."


waveviewWidth : Model -> Int
waveviewWidth model =
    model.width - Theme.sizes.rhythm * 3 - infoboxWidth - 10


infoboxWidth : Int
infoboxWidth =
    120


innerView : AudioData -> Model -> List String -> Element Msg
innerView audioData model playlist =
    let
        ( header, at ) =
            case model.playing of
                Playing from ->
                    ( [ el [ Font.weight Font.bold ] <| text Translations.playing
                      , Theme.button []
                            { onPress = Just PauseResume
                            , label = text Translations.pause
                            }
                      , Theme.button []
                            { onPress = Just Stop
                            , label = text Translations.stop
                            }
                      ]
                    , Duration.from from model.now
                    )

                Paused at_ ->
                    ( if at_ == Quantity.zero then
                        [ el [ Font.weight Font.bold ] <| text Translations.stopped
                        , Theme.button []
                            { onPress = Just Play
                            , label = text Translations.play
                            }
                        ]

                      else
                        [ el [ Font.weight Font.bold ] <| text Translations.paused
                        , Theme.button []
                            { onPress = Just PauseResume
                            , label = text Translations.resume
                            }
                        , Theme.button []
                            { onPress = Just Stop
                            , label = text Translations.stop
                            }
                        ]
                    , at_
                    )
    in
    column [ height fill ]
        [ menuBar
        , Ui.scrollable [ height fill ] <|
            Theme.column
                [ Theme.padding
                , height fill
                ]
                [ volumeSlider model.mainVolume
                , Theme.row [ width shrink ] header
                , timeTracker audioData model at
                , viewTracks audioData model at
                , addButtons playlist
                ]
        ]


viewTracks : AudioData -> Model -> Duration -> Element Msg
viewTracks audioData model at =
    let
        waveformConfig :
            { totalLength : Maybe Duration
            , at : Duration
            }
        waveformConfig =
            { totalLength = totalLength audioData model
            , at = at
            }
    in
    Table.view [ spacing 0, padding 0 ]
        (Table.columns
            [ Table.column
                { header = Table.cell [ padding 0 ] Ui.none
                , view =
                    \{ name } ->
                        Table.cell [ padding 0, width <| px infoboxWidth ] <|
                            Theme.column []
                                [ Ui.row [ spacing 2 ]
                                    [ el
                                        [ width <| px 20
                                        , height <| px 20
                                        , Ui.border 1
                                        ]
                                        (el
                                            [ centerX
                                            , Events.onClick (RemoveTrack name)
                                            , Ui.moveUp
                                                { x = 0
                                                , y = -6
                                                , z = 0
                                                }
                                            ]
                                            (textInvariant "X")
                                        )
                                    , el
                                        [ Ui.clipWithEllipsis
                                        , width <| px <| infoboxWidth - 22
                                        , Theme.titleInvariant name
                                        ]
                                        (textInvariant name)
                                    ]
                                , Ui.row []
                                    [ Theme.button []
                                        { onPress = Nothing
                                        , label = text Translations.mute
                                        }
                                    , Theme.button []
                                        { onPress = Nothing
                                        , label = text Translations.solo
                                        }
                                    ]
                                ]
                }
            , Table.column
                { header = Table.cell [ padding 0 ] Ui.none
                , view =
                    \track ->
                        Table.cell [ padding 0, width <| px <| waveviewWidth model ] <|
                            Ui.map WaveformMsg <|
                                View.Waveform.view waveformConfig track
                }
            ]
        )
        model.tracks


timeTracker : AudioData -> Model -> Duration -> Element msg
timeTracker audioData model at =
    case totalLength audioData model of
        Nothing ->
            Ui.none

        Just length ->
            el [ Font.family [ Font.monospace ] ] <|
                textInvariant <|
                    durationToString at
                        ++ " / "
                        ++ durationToString length


addButtons : List String -> Element Msg
addButtons =
    Lazy.lazy <|
        \playlist ->
            playlist
                |> List.map
                    (\name ->
                        Theme.button []
                            { label =
                                name
                                    |> Translations.add
                                    |> text
                            , onPress = Just <| AddTrack name
                            }
                    )
                |> (::) (el [ Font.weight Font.bold ] <| text Translations.loaded)
                |> Theme.row [ Ui.wrap, alignBottom ]


volumeSlider : Float -> Element Msg
volumeSlider =
    Lazy.lazy <|
        \mainVolume ->
            Ui.withContext <|
                \context ->
                    let
                        label :
                            { element : Element msg
                            , id : Label
                            }
                        label =
                            Input.label context "volume-label" [ Font.weight Font.bold ] (text Translations.mainVolume)
                    in
                    Theme.column []
                        [ label.element
                        , Theme.sliderHorizontal []
                            { onChange = Volume
                            , label = label.id
                            , min = 0
                            , max = 1
                            , step = Nothing
                            , value = mainVolume
                            }
                        ]


durationToString : Duration -> String
durationToString duration =
    let
        seconds : Float
        seconds =
            Float.Extra.modBy 60 (Duration.inSeconds duration)

        minutes : Int
        minutes =
            (Duration.inSeconds duration - seconds) / 60 |> floor
    in
    String.padLeft 2 '0' (String.fromInt minutes) ++ ":" ++ String.padLeft 5 '0' (Round.round 2 seconds)


menuBar : Element Msg
menuBar =
    Theme.row
        [ Theme.padding
        , Ui.background Theme.colors.gray
        ]
        [ el [ alignRight ] languagePicker ]


languagePicker : Element Msg
languagePicker =
    Ui.withContext <|
        \{ i18n } ->
            let
                current : Translations.Language
                current =
                    Translations.currentLanguage i18n

                languageToOption : Translations.Language -> Input.Option Translations.Language Msg
                languageToOption language =
                    Input.option language <| textInvariant <| Translations.languageToString language
            in
            Input.chooseOne Ui.row
                [ Theme.spacing ]
                { label = Input.labelHidden "Language"
                , onChange = SwitchLanguage
                , options = List.map languageToOption Translations.languages
                , selected = Just current
                }


subscriptions : AudioData -> Model -> Sub (Timed Msg)
subscriptions _ _ =
    Sub.batch
        [ gotAudioSummary (\data -> GotAudioSummary data |> Untimed)
        , Browser.Events.onAnimationFrame (\now -> Timed Tick now)
        , Browser.Events.onResize (\w h -> Resize w h |> Untimed)
        , Browser.Events.onKeyPress keypressDecoder
            |> Sub.map Untimed
        ]


keypressDecoder : Json.Decode.Decoder Msg
keypressDecoder =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\key ->
                case key of
                    " " ->
                        Json.Decode.succeed PauseResume

                    _ ->
                        Json.Decode.fail "Ignored"
            )
