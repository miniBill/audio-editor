module MyUi.Anim exposing (Animated, Duration, Msg, State, Step, Transition, hovered, hoveredWith, layout)

import Html exposing (Html)
import MyUi exposing (Attribute, Element)
import MyUi.Internal exposing (attribute, unwrapAttributes, unwrapElement)
import MyUi.Responsive
import Types exposing (Context)
import Ui.Anim


type alias Step =
    Ui.Anim.Step


type alias Animated =
    Ui.Anim.Animated


type alias Transition =
    Ui.Anim.Transition


type alias Duration =
    Ui.Anim.Duration


type alias Msg =
    Ui.Anim.Msg


type alias State =
    Ui.Anim.State


layout :
    Context
    ->
        { options : List MyUi.Option
        , toMsg : Msg -> msg
        , breakpoints : Maybe (MyUi.Responsive.Breakpoints label)
        }
    -> Ui.Anim.State
    -> List (Attribute msg)
    -> Element msg
    -> Html msg
layout context config state attrs child =
    Ui.Anim.layout config state (unwrapAttributes context attrs) (unwrapElement context child)


hoveredWith : List Step -> MyUi.Internal.Attribute msg
hoveredWith steps =
    attribute (Ui.Anim.hoveredWith steps)


hovered : Ui.Anim.Duration -> List Ui.Anim.Animated -> Attribute msg
hovered duraction animations =
    attribute (Ui.Anim.hovered duraction animations)
