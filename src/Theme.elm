module Theme exposing (Attribute, Context, Element, button, colors, column, fontSizes, padding, row, sizes, spacing, text, textInvariant, wrappedRow)

import Element.WithContext as Element exposing (Color, rgb, rgba)
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
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


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs =
    Element.row (spacing :: attrs)


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs =
    Element.wrappedRow (spacing :: attrs)


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    Element.column (spacing :: attrs)


button : List (Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
button attrs =
    Input.button (Border.width sizes.borderWidth :: padding :: attrs)


text : (Translations.I18n -> String) -> Element msg
text f =
    Element.with (\{ i18n } -> f i18n) Element.text


textInvariant : String -> Element msg
textInvariant =
    Element.text
