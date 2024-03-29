module MyUi exposing (Attribute, Color, Element, Length, alignBottom, alignRight, background, behindContent, border, centerX, centerY, column, el, fill, height, html, htmlAttribute, layout, map, noAttr, none, padding, px, rounded, row, spacing, text, width, withContext, wrap)

import Html
import MyUi.Internal exposing (attribute, container, element, singleContainer)
import Types exposing (Context)
import Ui


type alias Length =
    Ui.Length


type alias Color =
    Ui.Color


type alias Element msg =
    MyUi.Internal.Element msg


type alias Attribute msg =
    MyUi.Internal.Attribute msg


html : Html.Html msg -> Element msg
html child =
    element (Ui.html child)


map : (a -> b) -> Element a -> Element b
map f (MyUi.Internal.Element child) =
    MyUi.Internal.Element (\context -> Ui.map f (child context))


wrap : Attribute msg
wrap =
    attribute Ui.wrap


layout : Context -> List (Attribute msg) -> Element msg -> Html.Html msg
layout context attrs (MyUi.Internal.Element child) =
    Ui.layout (List.map (\(MyUi.Internal.Attribute a) -> a context) attrs) (child context)


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs children =
    container Ui.row attrs children


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs children =
    container Ui.column attrs children


el : List (Attribute msg) -> Element msg -> Element msg
el attrs child =
    singleContainer Ui.el attrs child


height : Length -> Attribute msg
height v =
    attribute (Ui.height v)


width : Length -> Attribute msg
width v =
    attribute (Ui.width v)


spacing : Int -> Attribute msg
spacing v =
    attribute (Ui.spacing v)


padding : Int -> Attribute msg
padding v =
    attribute (Ui.padding v)


border : Int -> Attribute msg
border v =
    attribute (Ui.border v)


rounded : Int -> Attribute msg
rounded v =
    attribute (Ui.rounded v)


background : Color -> Attribute msg
background v =
    attribute (Ui.background v)


alignBottom : Attribute msg
alignBottom =
    attribute Ui.alignBottom


alignRight : Attribute msg
alignRight =
    attribute Ui.alignRight


none : Element msg
none =
    element Ui.none


noAttr : Attribute msg
noAttr =
    attribute Ui.noAttr


behindContent : Element msg -> Attribute msg
behindContent (MyUi.Internal.Element child) =
    MyUi.Internal.Attribute (\context -> Ui.behindContent (child context))


text : String -> Element msg
text v =
    element (Ui.text v)


centerX : Attribute msg
centerX =
    attribute Ui.centerX


centerY : Attribute msg
centerY =
    attribute Ui.centerY


fill : Length
fill =
    Ui.fill


px : Int -> Length
px =
    Ui.px


htmlAttribute : Html.Attribute msg -> Attribute msg
htmlAttribute attr =
    attribute (Ui.htmlAttribute attr)


withContext : (Context -> Element msg) -> Element msg
withContext f =
    MyUi.Internal.Element
        (\context ->
            let
                (MyUi.Internal.Element child) =
                    f context
            in
            child context
        )
