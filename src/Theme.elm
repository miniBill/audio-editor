module Theme exposing (Attribute, Context, Element, button, colors, column, darken, fontSizes, grid, onCtrlEnter, onEnter, padding, row, sizes, spacing, text, textInvariant, wrappedRow)

import Color
import Element.WithContext as Element exposing (Color, el, none, rgb, rgba, shrink)
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Html.Events
import Json.Decode as Decode
import Translations


type alias Context =
    { i18n : Translations.I18n }


type alias Element msg =
    Element.Element Context msg


type alias Attribute msg =
    Element.Attribute Context msg


fontSizes :
    { normal : Attribute msg
    }
fontSizes =
    let
        modular : Int -> Attribute msg
        modular n =
            Font.size <| round <| Element.modular 20 1.25 n
    in
    { normal = modular 0
    }


sizes :
    { borderWidth : number1
    , roundness : number2
    , rhythm : number3
    }
sizes =
    { borderWidth = 1
    , roundness = 3
    , rhythm = 10
    }


spacing : Attribute msg
spacing =
    Element.spacing sizes.rhythm


padding : Attribute msg
padding =
    Element.padding sizes.rhythm


colors :
    { background : Color
    , errorMessage : Color
    , modalTransparentBackground : Color
    , warning : Color
    , gray : Color
    }
colors =
    { background = rgb 0.9 0.9 0.9
    , errorMessage = rgb 0.9 0 0
    , modalTransparentBackground = rgba 0.5 0.5 0.5 0.5
    , warning = rgb 0.8 0.8 0
    , gray = rgb 0.8 0.8 0.8
    }


darken : Color -> Color
darken =
    mapHsl (\c -> { c | lightness = 0.8 * c.lightness })


type alias Hsla =
    { hue : Float
    , saturation : Float
    , lightness : Float
    , alpha : Float
    }


mapHsl : (Hsla -> Hsla) -> Color -> Color
mapHsl f =
    Element.toRgb
        >> Color.fromRgba
        >> Color.toHsla
        >> f
        >> Color.fromHsla
        >> Color.toRgba
        >> Element.fromRgb


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs =
    Element.row (spacing :: attrs)


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs =
    Element.wrappedRow (spacing :: attrs)


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    Element.column (spacing :: attrs)


grid : List (Attribute msg) -> List (List (Element msg)) -> Element msg
grid attrs rows =
    if List.isEmpty rows then
        none

    else
        let
            toColumn : Int -> Element.Column Context (List (Element msg)) msg
            toColumn i =
                { width = shrink
                , header = none
                , view =
                    \x ->
                        x
                            |> List.drop i
                            |> List.head
                            |> Maybe.withDefault none
                            |> el [ Element.alignBottom ]
                }

            columnCount : Int
            columnCount =
                List.map List.length rows
                    |> List.maximum
                    |> Maybe.withDefault 0
        in
        Element.table (spacing :: attrs)
            { columns = List.map toColumn <| List.range 0 (columnCount - 1)
            , data = rows
            }


onEnter : msg -> Attribute msg
onEnter msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\s ->
                if s == "Enter" then
                    Decode.succeed msg

                else
                    Decode.fail "ignored"
            )
        |> Html.Events.on "keyup"
        |> Element.htmlAttribute


onCtrlEnter : msg -> Attribute msg
onCtrlEnter msg =
    Decode.map2 Tuple.pair
        (Decode.field "key" Decode.string)
        (Decode.field "ctrlKey" Decode.bool)
        |> Decode.andThen
            (\( s, ctrl ) ->
                if s == "Enter" && ctrl then
                    Decode.succeed msg

                else
                    Decode.fail "ignored"
            )
        |> Html.Events.on "keyup"
        |> Element.htmlAttribute


button : List (Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
button attrs =
    Input.button (Border.width sizes.borderWidth :: padding :: attrs)


text : (Translations.I18n -> String) -> Element msg
text f =
    Element.with (\{ i18n } -> f i18n) Element.text


textInvariant : String -> Element msg
textInvariant =
    Element.text
