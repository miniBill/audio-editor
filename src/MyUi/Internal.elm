module MyUi.Internal exposing (Attribute(..), Element(..), attribute, container, element, singleContainer, unwrapAttributes, unwrapElement, wrap)

import Types exposing (Context)
import Ui


type Attribute msg
    = Attribute (Context -> Ui.Attribute msg)


type Element msg
    = Element (Context -> Ui.Element msg)


container :
    (List (Ui.Attribute msg) -> List (Ui.Element msg) -> Ui.Element msg)
    -> List (Attribute msg)
    -> List (Element msg)
    -> Element msg
container orig attrs children =
    wrap orig attrs <| \context -> unwrapElements context children


singleContainer :
    (List (Ui.Attribute msg) -> Ui.Element msg -> Ui.Element msg)
    -> List (Attribute msg)
    -> Element msg
    -> Element msg
singleContainer orig attrs (Element child) =
    wrap orig attrs child


wrap : (List (Ui.Attribute msg) -> child -> Ui.Element msg) -> List (Attribute msg) -> (Context -> child) -> Element msg
wrap orig attrs child =
    Element (\context -> orig (unwrapAttributes context attrs) (child context))


unwrapElements : Context -> List (Element msg) -> List (Ui.Element msg)
unwrapElements context children =
    List.map (unwrapElement context) children


unwrapElement : Context -> (Element msg -> Ui.Element msg)
unwrapElement context (Element c) =
    c context


unwrapAttributes : Context -> List (Attribute msg) -> List (Ui.Attribute msg)
unwrapAttributes context attrs =
    List.map (unwrapAttribute context) attrs


unwrapAttribute : Context -> (Attribute msg -> Ui.Attribute msg)
unwrapAttribute context (Attribute a) =
    a context


element : Ui.Element msg -> Element msg
element orig =
    Element (\_ -> orig)


attribute : Ui.Attribute msg -> Attribute msg
attribute orig =
    Attribute (\_ -> orig)
