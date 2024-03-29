module MyUi.Table exposing (Cell, Column, Config, cell, column, columns, view, withWidth)

import MyUi exposing (Attribute, Element)
import MyUi.Internal exposing (unwrapAttributes, unwrapElement, wrap)
import Types exposing (Context)
import Ui.Table


type Config state data msg
    = Config (Context -> Ui.Table.Config state data msg)


type Column state data msg
    = Column (Context -> Ui.Table.Column state data msg)


type alias Cell msg =
    { attrs : List (Attribute msg)
    , child : Element msg
    }


view : List (Attribute msg) -> Config () data msg -> List data -> Element msg
view attrs (Config config) data =
    wrap (\a c -> Ui.Table.view a c data) attrs config


columns : List (Column state data msg) -> Config state data msg
columns cols =
    Config (\context -> Ui.Table.columns (List.map (\(Column c) -> c context) cols))


column : { header : Cell msg, view : data -> Cell msg } -> Column state data msg
column config =
    Column
        (\context ->
            Ui.Table.column
                { header = unwrapCell context config.header
                , view = \data -> unwrapCell context (config.view data)
                }
        )


unwrapCell : Context -> Cell msg -> Ui.Table.Cell msg
unwrapCell context { attrs, child } =
    { attrs = unwrapAttributes context attrs
    , child = unwrapElement context child
    }


withWidth :
    { fill : Bool, min : Maybe Int, max : Maybe Int }
    -> Column state data msg
    -> Column state data msg
withWidth config (Column col) =
    Column (\context -> col context |> Ui.Table.withWidth config)


cell : List (Attribute msg) -> Element msg -> Cell msg
cell attrs child =
    { attrs = attrs
    , child = child
    }
