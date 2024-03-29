module MyUi exposing (Attribute, Color, Element, Length, Position, alignBottom, alignRight, background, behindContent, border, centerX, centerY, clipWithEllipsis, column, el, fill, height, html, htmlAttribute, layout, map, move, noAttr, none, padding, px, rounded, row, scrollable, shrink, spacing, text, title, width, withContext, wrap)

import Html
import Html.Attributes
import MyUi.Internal exposing (attribute, container, element, singleContainer, unwrapAttributes, unwrapElement)
import Types exposing (Context)
import Ui


type alias Length =
    Ui.Length


type alias Color =
    Ui.Color


type alias Position =
    Ui.Position


type alias Element msg =
    MyUi.Internal.Element msg


type alias Attribute msg =
    MyUi.Internal.Attribute msg


html : Html.Html msg -> Element msg
html child =
    element (Ui.html child)


map : (a -> b) -> Element a -> Element b
map f child =
    MyUi.Internal.Element (\context -> Ui.map f (unwrapElement context child))


wrap : Attribute msg
wrap =
    attribute Ui.wrap


layout : Context -> List (Attribute msg) -> Element msg -> Html.Html msg
layout context attrs child =
    Ui.layout (unwrapAttributes context attrs) (unwrapElement context child)


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs children =
    container Ui.row attrs children


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs children =
    container Ui.column attrs children


scrollable : List (Attribute msg) -> Element msg -> Element msg
scrollable attrs child =
    singleContainer Ui.scrollable attrs child


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


title : String -> Attribute msg
title value =
    attribute (Ui.htmlAttribute (Html.Attributes.title value))


behindContent : Element msg -> Attribute msg
behindContent child =
    MyUi.Internal.Attribute (\context -> Ui.behindContent (unwrapElement context child))


text : String -> Element msg
text v =
    element (Ui.text v)


centerX : Attribute msg
centerX =
    attribute Ui.centerX


centerY : Attribute msg
centerY =
    attribute Ui.centerY


move : Position -> Attribute msg
move value =
    attribute (Ui.move value)


clipWithEllipsis : Attribute msg
clipWithEllipsis =
    attribute Ui.clipWithEllipsis


fill : Length
fill =
    Ui.fill


shrink : Length
shrink =
    Ui.shrink


px : Int -> Length
px =
    Ui.px


htmlAttribute : Html.Attribute msg -> Attribute msg
htmlAttribute attr =
    attribute (Ui.htmlAttribute attr)


withContext : (Context -> Element msg) -> Element msg
withContext f =
    MyUi.Internal.Element
        (\context -> unwrapElement context (f context))
