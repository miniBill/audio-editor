module Types exposing (Point, RawData)


type alias RawData =
    List (List Point)


type alias Point =
    ( Float, Float, Float )
