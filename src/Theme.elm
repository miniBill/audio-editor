module Theme exposing (button, colors, column, fontSizes, icon, padding, rhythm, row, sliderHorizontal, spacing, text, textInvariant, titleInvariant, toggleButton)

import Color exposing (rgb)
import Html.Attributes
import MyUi exposing (Attribute, Color, Element, fill, height, px, shrink, width)
import MyUi.Anim
import MyUi.Events
import MyUi.Font as Font
import MyUi.Input as Input
import Phosphor
import Translations
import Ui.Anim


fontSizes :
    { normal : Attribute msg
    }
fontSizes =
    { normal = Font.size 14
    }


rhythm : number
rhythm =
    4


spacing : Attribute msg
spacing =
    MyUi.spacing rhythm


padding : Attribute msg
padding =
    MyUi.padding rhythm


colors :
    { background : Color
    , gray : Color
    }
colors =
    { background = rgb 0.9 0.9 0.9
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
        (MyUi.border 1
            :: padding
            :: width shrink
            :: (case config.onPress of
                    Just msg ->
                        MyUi.Events.onClick msg

                    Nothing ->
                        MyUi.noAttr
               )
            :: MyUi.Anim.hovered durations.short
                [ Ui.Anim.backgroundColor <| rgb 0.8 0.8 1
                ]
            :: attrs
        )
        config.label


durations : { short : Ui.Anim.Duration }
durations =
    { short = Ui.Anim.ms 150 }


toggleButton : List (Attribute msg) -> { active : Bool, onPress : Maybe msg, label : Element msg } -> Element msg
toggleButton attrs config =
    button
        ((if config.active then
            MyUi.background <| rgb 0.4 0.4 0.9

          else
            MyUi.noAttr
         )
            :: attrs
        )
        { onPress = config.onPress
        , label = config.label
        }


text : (Translations.I18n -> String) -> Element msg
text f =
    MyUi.withContext (\{ i18n } -> MyUi.text (f i18n))


textInvariant : String -> Element msg
textInvariant value =
    MyUi.text value


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


titleInvariant : String -> Attribute msg
titleInvariant value =
    MyUi.title value


icon : Phosphor.Icon -> Element msg
icon value =
    value Phosphor.Duotone
        |> Phosphor.toHtml [ Html.Attributes.style "color" "blue" ]
        |> MyUi.html
