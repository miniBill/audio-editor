module Types exposing (AudioSummary, Context, Point)

import Translations


type alias AudioSummary =
    List (List Point)


type alias Point =
    ( Float, Float, Float )


type alias Context =
    { i18n : Translations.I18n }
