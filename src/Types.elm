module Types exposing (AudioSummary, Context, Point, Selection(..), SelectionData, Track)

import Audio
import Duration exposing (Duration)
import Translations


type alias AudioSummary =
    List (List Point)


type alias Point =
    ( Float, Float, Float )


type alias Context =
    { i18n : Translations.I18n }


type alias Track =
    { name : String
    , url : String
    , source : Audio.Source
    , summary : Maybe AudioSummary
    , offset : Duration
    , duration : Duration
    , mute : Bool
    , solo : Bool
    }


type Selection
    = SelectionNone
    | SelectionRange SelectionData


type alias SelectionData =
    { fromTrack : Int
    , toTrack : Int
    , from : Duration
    , to : Duration
    }
