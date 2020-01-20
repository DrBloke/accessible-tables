module Main exposing (simpleTable)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


type TableConfiguration data msg
    = ValidTable (Table data msg)
    | MalformedTable String


type Table data msg
    = Table
        { columnns : List Column
        , rows : List Row
        , caption : Maybe (Html msg)
        , summary : Maybe (Html msg)
        , cells : List (List (Cell data msg))
        , attributes : List (Html.Attribute msg)
        }


type Column
    = Column ColumnSettings (Maybe ColumnHeader)


type ColumnSettings
    = ColumnSettingsSingle
    | ColumnSettingsGroup (List ColumnSettings)


type ColumnSettingsSingle msg
    = ColumnSettings
        { attributes : List (Html.Attribute msg)
        , span : Int
        }


type ColumnHeader
    = ColumnHeaderSingle ColumnHeader
    | ColumnHeaderGroup (List ColumnHeader)


type ColumnHeaderSingle data msg
    = ColumnHeader
        { label : data -> Html msg
        , attributes : List (Html.Attribute msg)
        }


type Row
    = Row RowSettings (Maybe RowHeader)


type RowSettings
    = RowSettingsSingle
    | RowSettingsGroup (List RowSettings)


type RowSettingsSingle msg
    = RowSettings
        { attributes : List (Html.Attribute msg)
        , span : Int
        }


type RowHeader
    = RowHeaderSingle RowHeader
    | RowHeaderGroup (List RowHeader)


type RowHeaderSingle data msg
    = RowHeader
        { label : data -> Html msg
        , attributes : List (Html.Attribute msg)
        }


type Cell data msg
    = Cell
        { value : data -> Html msg
        , attributes : List (Html.Attribute msg)
        }


simpleTable : List (List String) -> TableConfiguration data msg
simpleTable data =
    ValidTable
        (Table
            { columnns =
                case data of
                    [ [] ] ->
                        [ [] ]

                    rows ->
                        List.repeat (List.length rows)
                            (Row
                                (RowSettings { attributes = [], span = 1 })
                                Nothing
                            )
            , rows =
                case data of
                    [ [] ] ->
                        [ [] ]

                    [ column1 :: otherColumns ] ->
                        List.repeat (List.length column1)
                            Column
                            (ColumnSettings { attributes = [], span = 1 })
                            Nothing
            , caption = Nothing
            , summary = Nothing
            , cells =
                case data of
                    [ [] ] ->
                        [ [] ]

                    [ xs ] ->
                        [ List.map (\v -> Cell { value = Html.text v, attributes = [] }) xs ]
            , attributes = []
            }
        )
