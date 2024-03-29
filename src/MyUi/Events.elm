module MyUi.Events exposing (onClick)

import MyUi exposing (Attribute)
import MyUi.Internal
import Ui.Events


onClick : msg -> Attribute msg
onClick msg =
    MyUi.Internal.attribute (Ui.Events.onClick msg)
