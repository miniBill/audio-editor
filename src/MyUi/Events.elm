module MyUi.Events exposing (onClick)

import MyUi exposing (Attribute)
import MyUi.Internal exposing (attribute)
import Ui.Events


onClick : msg -> Attribute msg
onClick msg =
    attribute (Ui.Events.onClick msg)
