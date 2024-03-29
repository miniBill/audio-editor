module MyUi.Lazy exposing (lazy)

import MyUi exposing (Element)
import MyUi.Internal
import Types exposing (Context)
import Ui
import Ui.Lazy


lazy : (a -> Element msg) -> a -> Element msg
lazy f arg =
    MyUi.Internal.Element (\context -> Ui.Lazy.lazy3 helper f context arg)


helper : (value -> Element msg) -> Context -> value -> Ui.Element msg
helper f context arg =
    let
        (MyUi.Internal.Element child) =
            f arg
    in
    child context
