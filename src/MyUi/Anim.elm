module MyUi.Anim exposing (Animated, Duration, Msg, State, Step, backgroundColor, hoveredWith, init, layout, ms, step, subscription, update)

import Html exposing (Html)
import MyUi exposing (Attribute, Color, Element)
import MyUi.Internal exposing (attribute, unwrapAttributes, unwrapElement)
import MyUi.Responsive
import Types exposing (Context)
import Ui.Anim


type alias Step =
    Ui.Anim.Step


type alias Animated =
    Ui.Anim.Animated


type alias Duration =
    Ui.Anim.Duration


type alias Msg =
    Ui.Anim.Msg


type alias State =
    Ui.Anim.State


type alias Animator msg model =
    Ui.Anim.Animator msg model


layout :
    Context
    ->
        { options : List MyUi.Option
        , toMsg : Msg -> msg
        , breakpoints : Maybe (MyUi.Responsive.Breakpoints label)
        }
    -> State
    -> List (Attribute msg)
    -> Element msg
    -> Html msg
layout context config state attrs child =
    Ui.Anim.layout config state (unwrapAttributes context attrs) (unwrapElement context child)


init : State
init =
    Ui.Anim.init


update : (Msg -> msg) -> Msg -> State -> ( State, Cmd msg )
update =
    Ui.Anim.update


subscription : (Msg -> msg) -> State -> Animator msg model -> model -> Sub msg
subscription =
    Ui.Anim.subscription


backgroundColor : Color -> Animated
backgroundColor =
    Ui.Anim.backgroundColor


hoveredWith : List Step -> MyUi.Internal.Attribute msg
hoveredWith steps =
    attribute (Ui.Anim.hoveredWith steps)


step : Duration -> List Animated -> Step
step =
    Ui.Anim.step


ms : Float -> Duration
ms =
    Ui.Anim.ms
