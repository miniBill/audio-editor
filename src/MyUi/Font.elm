module MyUi.Font exposing (Font, Weight, bold, family, monospace, size, weight)

import MyUi exposing (Attribute)
import MyUi.Internal exposing (attribute)
import Ui.Font


type alias Weight =
    Ui.Font.Weight


type alias Font =
    Ui.Font.Font


weight : Weight -> Attribute msg
weight v =
    attribute (Ui.Font.weight v)


size : Int -> Attribute msg
size v =
    attribute (Ui.Font.size v)


bold : Weight
bold =
    Ui.Font.bold


family : List Font -> Attribute msg
family values =
    attribute (Ui.Font.family values)


monospace : Font
monospace =
    Ui.Font.monospace
