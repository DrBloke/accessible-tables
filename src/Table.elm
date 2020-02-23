module Table exposing (render, setColumnHeadings, simpleTable)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


type TableConfiguration msg
    = ValidTable (Table msg)
    | MalformedTable String


type Table msg
    = Table
        { columnHeaders : List (ColumnHeaders msg)
        , rowHeaders : List (RowHeaders msg)
        , caption : Maybe (Html msg)
        , summary : Maybe (Html msg)
        , cells : List (List (Cell msg))
        , attributes : List (Html.Attribute msg)
        }


type ColumnHeaders msg
    = NoColumnHeaders
    | ColumnHeaderSingle (ColumnHeader msg)
    | ColumnHeaderGroup (ColumnHeader msg) (List (ColumnHeader msg))


type ColumnHeader msg
    = ColumnHeader
        { label : Html msg
        , attributes : List (Html.Attribute msg)
        }


type RowHeaders msg
    = NoRowHeaders
    | RowHeaderSingle (RowHeader msg)
    | RowHeaderGroup (RowHeader msg) (List (RowHeader msg))


type RowHeader msg
    = RowHeader
        { label : Html msg
        , attributes : List (Html.Attribute msg)
        }


type Cell msg
    = Cell
        { value : Html msg
        , attributes : List (Html.Attribute msg)
        }


simpleTable : List (List String) -> TableConfiguration msg
simpleTable data =
    --do a check that all rows have equal number of cells
    ValidTable
        (Table
            { columnHeaders =
                case data of
                    [] ->
                        [ NoColumnHeaders ]

                    rows ->
                        let
                            noOfColumns =
                                List.head rows
                                    |> Maybe.withDefault []
                                    |> List.length
                        in
                        case noOfColumns of
                            0 ->
                                [ NoColumnHeaders ]

                            _ ->
                                List.repeat noOfColumns
                                    NoColumnHeaders
            , rowHeaders =
                case data of
                    [] ->
                        [ NoRowHeaders ]

                    rows ->
                        List.repeat (List.length rows)
                            NoRowHeaders
            , caption = Nothing
            , summary = Nothing
            , cells =
                case data of
                    rows ->
                        List.map (\row -> List.map (\v -> Cell { value = Html.text v, attributes = [] }) row) rows
            , attributes = []
            }
        )


setColumnHeadings : List String -> TableConfiguration msg -> TableConfiguration msg
setColumnHeadings columns config =
    --check that number equals number of columns
    case config of
        ValidTable (Table tableConfig) ->
            ValidTable
                (Table
                    { tableConfig
                        | columnHeaders =
                            List.map
                                (\label ->
                                    ColumnHeaderSingle
                                        (ColumnHeader
                                            { label = Html.text label
                                            , attributes = []
                                            }
                                        )
                                )
                                columns
                    }
                )

        MalformedTable error ->
            MalformedTable error


render : TableConfiguration msg -> Html msg
render config =
    case config of
        ValidTable (Table tableConfig) ->
            let
                _ =
                    Debug.log "headers" tableConfig.columnHeaders

                columnHeadings =
                    [ thead []
                        [ tr []
                            (List.map
                                (\heading ->
                                    case heading of
                                        NoColumnHeaders ->
                                            th [] [ text "" ]

                                        ColumnHeaderSingle (ColumnHeader colHeader) ->
                                            th [] [ colHeader.label ]

                                        ColumnHeaderGroup _ _ ->
                                            th [] [ text "" ]
                                )
                                tableConfig.columnHeaders
                            )
                        ]
                    ]

                cells =
                    [ tbody []
                        (List.map
                            (\row ->
                                tr [] (List.map (\(Cell cell) -> td [] [ cell.value ]) row)
                            )
                            tableConfig.cells
                        )
                    ]
            in
            table []
                (columnHeadings
                    ++ cells
                )

        MalformedTable error ->
            text error
