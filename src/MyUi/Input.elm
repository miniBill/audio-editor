module MyUi.Input exposing (Label, Option, chooseOne, label, labelHidden, option, sliderHorizontal)

import MyUi exposing (Attribute, Element)
import MyUi.Internal exposing (attribute, element, unwrapAttributes, wrap)
import Types exposing (Context)
import Ui
import Ui.Input


type alias Label =
    Ui.Input.Label


type Option value msg
    = Option (Context -> Ui.Input.Option value msg)


sliderHorizontal :
    List (Attribute msg)
    ->
        { label : Label
        , onChange : Float -> msg
        , min : Float
        , max : Float
        , value : Float
        , thumb : Maybe (Ui.Input.Thumb msg)
        , step : Maybe Float
        }
    -> Element msg
sliderHorizontal attrs config =
    wrap Ui.Input.sliderHorizontal attrs <| \_ -> config


chooseOne :
    (List (Attribute msg) -> List (Element msg) -> Element msg)
    -> List (Attribute msg)
    ->
        { onChange : option -> msg
        , options : List (Option option msg)
        , selected : Maybe option
        , label : Label
        }
    -> Element msg
chooseOne layout attrs config =
    MyUi.Internal.Element
        (\context ->
            Ui.Input.chooseOne
                (\layoutAttrs layoutChildren ->
                    let
                        (MyUi.Internal.Element l) =
                            layout
                                (List.map attribute layoutAttrs)
                                (List.map element layoutChildren)
                    in
                    l context
                )
                (unwrapAttributes context attrs)
                { onChange = config.onChange
                , label = config.label
                , options = List.map (\(Option o) -> o context) config.options
                , selected = config.selected
                }
        )


option : value -> Element msg -> Option value msg
option value (MyUi.Internal.Element child) =
    Option (\context -> Ui.Input.option value (child context))


label :
    Context
    -> String
    -> List (Attribute msg)
    -> Element msg
    ->
        { element : Element msg
        , id : Ui.Input.Label
        }
label context id attrs (MyUi.Internal.Element labelElement) =
    let
        inner : { element : Ui.Element msg, id : Ui.Input.Label }
        inner =
            Ui.Input.label id (unwrapAttributes context attrs) (labelElement context)
    in
    { element = element inner.element
    , id = inner.id
    }


labelHidden : String -> Label
labelHidden content =
    Ui.Input.labelHidden content
