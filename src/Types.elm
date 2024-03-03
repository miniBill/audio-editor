module Types exposing (AudioSummary, Point)


type alias AudioSummary =
    List (List Point)


type alias Point =
    ( Float, Float, Float )
