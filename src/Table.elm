module Table exposing
    ( hideColumnHeaders
    , hideRowHeaders
    , render
    , setColumnHeadings
    , setRowHeadings
    , simpleTable
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List


type TableConfiguration msg
    = ValidTable (Table msg)
    | MalformedTable String


type Table msg
    = Table
        { columnHeaders : ColumnHeaders msg
        , columnHeadersShown : Bool
        , rowHeaders : RowHeaders msg
        , rowHeadersShown : Bool
        , caption : Maybe (Html msg)
        , summary : Maybe (Html msg)
        , cells : List (List (Cell msg))
        , attributes : List (Html.Attribute msg)
        }


type ColumnHeaders msg
    = ColumnHeadersSimple (List (ColumnHeaderSingle msg))
    | ColumnHeadersComplex (List (ColumnHeaderGroup msg))


type ColumnHeaderSingle msg
    = ColumnHeaderSingle (ColumnHeader msg)


type ColumnHeaderGroup msg
    = ColumnHeaderGroup (ColumnHeader msg) (List (ColumnHeader msg))


type ColumnHeader msg
    = ColumnHeader
        { label : Html msg
        , attributes : List (Html.Attribute msg)
        }


type RowHeaders msg
    = RowHeadersSimple (List (RowHeaderSingle msg))
    | RowHeadersComplex (List (RowHeaderGroup msg))


type RowHeaderSingle msg
    = RowHeaderSingle (RowHeader msg)


type RowHeaderGroup msg
    = RowHeaderGroup (RowHeader msg) (List (RowHeader msg))


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
    -- do a check that all rows have equal number of cells
    case allRowsEqualLength data of
        Ok noOfCols ->
            ValidTable
                (Table
                    { columnHeaders = ColumnHeadersSimple (List.range 1 noOfCols |> List.map (\colNo -> ColumnHeaderSingle (ColumnHeader { label = text (String.fromInt colNo), attributes = [] })))
                    , columnHeadersShown = True
                    , rowHeaders = RowHeadersSimple (List.range 1 (List.length data) |> List.map (\rowNo -> RowHeaderSingle (RowHeader { label = text (String.fromInt rowNo), attributes = [] })))
                    , rowHeadersShown = True
                    , caption = Nothing
                    , summary = Nothing
                    , cells =
                        List.map (\row -> List.map (\v -> Cell { value = Html.text v, attributes = [] }) row) data
                    , attributes = []
                    }
                )

        Err error ->
            MalformedTable error


allRowsEqualLength : List (List String) -> Result String Int
allRowsEqualLength data =
    case data of
        [] ->
            Ok 0

        row1 :: remainingRows ->
            allRowsEqualLengthHelper (List.length row1) remainingRows


allRowsEqualLengthHelper : Int -> List (List a) -> Result String Int
allRowsEqualLengthHelper rowLength remainingRows =
    case remainingRows of
        [] ->
            Ok rowLength

        thisRow :: rest ->
            if List.length thisRow == rowLength then
                allRowsEqualLengthHelper rowLength rest

            else
                Err "row lengths don't match"


hideColumnHeaders : TableConfiguration msg -> TableConfiguration msg
hideColumnHeaders config =
    case config of
        MalformedTable error ->
            MalformedTable error

        ValidTable (Table tableConfig) ->
            ValidTable
                (Table
                    { tableConfig | columnHeadersShown = False }
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
                            ColumnHeadersSimple
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
                            RowHeadersSimple
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


hideRowHeaders : TableConfiguration msg -> TableConfiguration msg
hideRowHeaders config =
    case config of
        MalformedTable error ->
            MalformedTable error

        ValidTable (Table tableConfig) ->
            ValidTable
                (Table
                    { tableConfig | rowHeadersShown = False }
                )


render : TableConfiguration msg -> Result String (Html msg)
render config =
    case config of
        ValidTable (Table tableConfig) ->
            case tableConfig.cells of
                [ [] ] ->
                    Err "no data"

                _ ->
                    let
                        spacer =
                            if tableConfig.rowHeadersShown then
                                [ td [] [] ]

                            else
                                []

                        columnHeadings =
                            case tableConfig.columnHeaders of
                                ColumnHeadersSimple cols ->
                                    [ thead []
                                        [ tr [ hidden (not tableConfig.columnHeadersShown) ]
                                            (spacer
                                                ++ List.map
                                                    (\(ColumnHeaderSingle (ColumnHeader colInfo)) ->
                                                        th [ scope "col" ] [ colInfo.label ]
                                                    )
                                                    cols
                                            )
                                        ]
                                    ]

                                _ ->
                                    [ text "TODO complex" ]

                        body =
                            case tableConfig.rowHeaders of
                                RowHeadersSimple rowHeaders ->
                                    [ tbody []
                                        (List.zip tableConfig.cells rowHeaders
                                            |> List.map
                                                (\( rowDataInfo, RowHeaderSingle (RowHeader rowHeaderInfo) ) ->
                                                    tr [] ([ th ([ scope "row", hidden (not tableConfig.rowHeadersShown) ] ++ rowHeaderInfo.attributes) [ rowHeaderInfo.label ] ] ++ List.map (\(Cell cell) -> td cell.attributes [ cell.value ]) rowDataInfo)
                                                )
                                        )
                                    ]

                                RowHeadersComplex _ ->
                                    [ tbody [] [ tr [] [ td [] [ text "not implemented" ] ] ] ]
                    in
                    Ok
                        (table []
                            (columnHeadings
                                ++ body
                            )
                        )

        MalformedTable error ->
            Err error
