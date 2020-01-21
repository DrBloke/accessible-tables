module Main exposing (simpleTable)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


type TableConfiguration data msg
    = ValidTable (Table data msg)
    | MalformedTable String


type Table data msg
    = Table
        { columnns : List (Column data msg)
        , rows : List (Row data msg)
        , caption : Maybe (Html msg)
        , summary : Maybe (Html msg)
        , cells : List (List (Cell data msg))
        , attributes : List (Html.Attribute msg)
        }


type Column data msg
    = Column (ColumnSettings msg) (Maybe (ColumnHeader data msg))


type ColumnSettings msg
    = EmptyColumn
    | ColumnSettingsSingle (ColumnSettingsSingle msg)
    | ColumnSettingsGroup (List (ColumnSettings msg))


type ColumnSettingsSingle msg
    = ColumnSettings
        { attributes : List (Html.Attribute msg)
        , span : Int
        }


type ColumnHeader data msg
    = ColumnHeaderSingle (ColumnHeader data msg)
    | ColumnHeaderGroup (List (ColumnHeader data msg))


type ColumnHeaderSingle data msg
    = ColumnHeader
        { label : data -> Html msg
        , attributes : List (Html.Attribute msg)
        }


type Row data msg
    = Row (RowSettings msg) (Maybe (RowHeader data msg))


type RowSettings msg
    = EmptyRow
    | RowSettingsSingle (RowSettingsSingle msg)
    | RowSettingsGroup (List (RowSettings msg))


type RowSettingsSingle msg
    = RowSettings
        { attributes : List (Html.Attribute msg)
        , span : Int
        }


type RowHeader data msg
    = RowHeaderSingle (RowHeader data msg)
    | RowHeaderGroup (List (RowHeader data msg))


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
                    [ columns ] ->
                        List.repeat (List.length columns)
                            (Column
                                (ColumnSettingsSingle (ColumnSettings { attributes = [], span = 1 }))
                                Nothing
                            )

                    _ ->
                        [ Column EmptyColumn Nothing ]
            , rows =
                case data of
                    rows ->
                        List.repeat (List.length rows)
                            (Row
                                (RowSettingsSingle (RowSettings { attributes = [], span = 1 }))
                                Nothing
                            )

                    _ ->
                        [ Row EmptyRow Nothing ]
            , caption = Nothing
            , summary = Nothing
            , cells =
                case data of
                    [ xs ] ->
                        [ List.map (\v -> Cell { value = always (Html.text v), attributes = [] }) xs ]

                    _ ->
                        [ [] ]
            , attributes = []
            }
        )
