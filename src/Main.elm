module Main exposing (Flags, InnerModel, Model, Msg, main)

import Browser
import Element.WithContext as Element exposing (centerX, centerY, el, fill, height, width)
import Http
import Theme exposing (Context, Element, column, text)
import Translations


type alias Flags =
    { language : String }


type alias Model =
    { context : Context
    , inner : InnerModel
    }


type InnerModel
    = Loading
    | Loaded LoadedModel
    | LoadingError Http.Error


type alias LoadedModel =
    {}


type Msg
    = LoadedTranslation (Result Http.Error Translations.I18n)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view =
            \model ->
                Element.layout model.context
                    [ Theme.fontSizes.normal
                    , width fill
                    , height fill
                    ]
                    (view model)
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        i18n : Translations.I18n
        i18n =
            Translations.init
                { lang = Translations.En
                , path = "/dist/i18n"
                }
    in
    ( { context = { i18n = i18n }, inner = Loading }
    , Translations.loadMain
        (\result ->
            result
                |> Result.map (\f -> f i18n)
                |> LoadedTranslation
        )
        i18n
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ context } as model) =
    case msg of
        LoadedTranslation (Err e) ->
            ( { model | inner = LoadingError e }, Cmd.none )

        LoadedTranslation (Ok i18n) ->
            ( { model | context = { context | i18n = i18n }, inner = Loaded {} }, Cmd.none )


view : Model -> Element Msg
view model =
    case model.inner of
        Loading ->
            el [ centerX, centerY ] <| text <| \_ -> "Loading..."

        Loaded loadedModel ->
            innerView loadedModel

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


innerView : LoadedModel -> Element Msg
innerView _ =
    text Translations.hello


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
