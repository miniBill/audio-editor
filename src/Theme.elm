module Theme exposing (button, colors, column, fontSizes, padding, row, sizes, sliderHorizontal, spacing, text, textInvariant)

import Color exposing (rgb, rgba)
import MyUi exposing (Attribute, Color, Element, fill, height, px, width)
import MyUi.Events
import MyUi.Font as Font
import MyUi.Input as Input
import MyUi.Internal
import Translations
import Ui


fontSizes :
    { normal : Attribute msg
    }
fontSizes =
    { normal = Font.size 20
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
    MyUi.spacing sizes.rhythm


padding : Attribute msg
padding =
    MyUi.padding sizes.rhythm


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
    MyUi.row (spacing :: attrs)


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    MyUi.column (spacing :: attrs)


button : List (Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
button attrs config =
    MyUi.el
        (MyUi.border sizes.borderWidth
            :: padding
            :: (case config.onPress of
                    Just msg ->
                        MyUi.Events.onClick msg

                    Nothing ->
                        MyUi.noAttr
               )
            :: attrs
        )
        config.label


text : (Translations.I18n -> String) -> Element msg
text f =
    MyUi.Internal.Element (\{ i18n } -> Ui.text (f i18n))


textInvariant : String -> Element msg
textInvariant value =
    MyUi.Internal.Element (\_ -> Ui.text value)


sliderHorizontal :
    List (Attribute msg)
    ->
        { onChange : Float -> msg
        , label : Input.Label
        , min : Float
        , max : Float
        , step : Maybe Float
        , value : Float
        }
    -> Element msg
sliderHorizontal attrs config =
    Input.sliderHorizontal
        ([ height <| px 30
         , width fill
         , MyUi.behindContent
            (MyUi.el
                [ width fill
                , height <| px 2
                , MyUi.centerY
                , MyUi.background colors.gray
                , MyUi.rounded 2
                ]
                MyUi.none
            )
         ]
            ++ attrs
        )
        { onChange = config.onChange
        , label = config.label
        , min = config.min
        , max = config.max
        , step = config.step
        , value = config.value
        , thumb = Nothing
        }
