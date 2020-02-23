module Table exposing
    ( columHeadersNotRequired
    , render
    , setColumnHeadings
    , simpleTable
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


type TableConfiguration msg
    = ValidTable (Table msg)
    | MalformedTable String


type Table msg
    = Table
        { columnHeaders : Maybe (List (ColumnHeaders msg))
        , columnHeadersRequired : Bool
        , rowHeaders : Maybe (List (RowHeaders msg))
        , rowHeadersRequired : Bool
        , caption : Maybe (Html msg)
        , summary : Maybe (Html msg)
        , cells : List (List (Cell msg))
        , attributes : List (Html.Attribute msg)
        }


type ColumnHeaders msg
    = ColumnHeaderSingle (ColumnHeader msg)
    | ColumnHeaderGroup (ColumnHeader msg) (List (ColumnHeader msg))


type ColumnHeader msg
    = ColumnHeader
        { label : Html msg
        , attributes : List (Html.Attribute msg)
        }


type RowHeaders msg
    = RowHeaderSingle (RowHeader msg)
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
            { columnHeaders = Nothing
            , columnHeadersRequired = True
            , rowHeaders = Nothing
            , rowHeadersRequired = True
            , caption = Nothing
            , summary = Nothing
            , cells =
                case data of
                    rows ->
                        List.map (\row -> List.map (\v -> Cell { value = Html.text v, attributes = [] }) row) rows
            , attributes = []
            }
        )


columHeadersNotRequired : TableConfiguration msg -> TableConfiguration msg
columHeadersNotRequired config =
    case config of
        MalformedTable error ->
            MalformedTable error

        ValidTable (Table tableConfig) ->
            ValidTable
                (Table
                    { tableConfig | columnHeadersRequired = False }
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
                            Just
                                (List.map
                                    (\label ->
                                        ColumnHeaderSingle
                                            (ColumnHeader
                                                { label = Html.text label
                                                , attributes = []
                                                }
                                            )
                                    )
                                    columns
                                )
                    }
                )

        MalformedTable error ->
            MalformedTable error


setRowHeadings : List String -> TableConfiguration msg -> TableConfiguration msg
setRowHeadings rows config =
    --check that number equals number of rows
    case config of
        ValidTable (Table tableConfig) ->
            ValidTable
                (Table
                    { tableConfig
                        | rowHeaders =
                            Just
                                (List.map
                                    (\label ->
                                        RowHeaderSingle
                                            (RowHeader
                                                { label = Html.text label
                                                , attributes = []
                                                }
                                            )
                                    )
                                    rows
                                )
                    }
                )

        MalformedTable error ->
            MalformedTable error


validateColumns : TableConfiguration msg -> TableConfiguration msg
validateColumns config =
    case config of
        MalformedTable error ->
            MalformedTable error

        ValidTable (Table validConfig) ->
            case validConfig.columnHeaders of
                Nothing ->
                    if validConfig.columnHeadersRequired then
                        MalformedTable "Column headers are required"

                    else
                        let
                            noOfColumns =
                                case List.head validConfig.cells of
                                    Nothing ->
                                        0

                                    Just row ->
                                        List.length row
                        in
                        ValidTable
                            (Table
                                { validConfig
                                    | columnHeaders =
                                        Just
                                            (List.repeat noOfColumns
                                                (ColumnHeaderSingle
                                                    (ColumnHeader
                                                        { label = text ""
                                                        , attributes = []
                                                        }
                                                    )
                                                )
                                            )
                                }
                            )

                Just _ ->
                    ValidTable (Table validConfig)


render : TableConfiguration msg -> Html msg
render config =
    let
        validatedConfig =
            config
                |> validateColumns
    in
    case validatedConfig of
        ValidTable (Table tableConfig) ->
            case tableConfig.cells of
                [ [] ] ->
                    text "no data"

                _ ->
                    let
                        columnHeadings =
                            case tableConfig.columnHeaders of
                                Nothing ->
                                    [ text "invalid state" ]

                                Just colHeaders ->
                                    [ thead []
                                        [ tr []
                                            (List.map
                                                (\colHeader_ ->
                                                    case colHeader_ of
                                                        ColumnHeaderSingle (ColumnHeader colHeader) ->
                                                            th [ scope "col" ] [ colHeader.label ]

                                                        _ ->
                                                            text "complex tables not yet implemented"
                                                )
                                                colHeaders
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
