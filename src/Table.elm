module Table exposing
    ( TableError(..)
    , errorToString
    , hideColumnHeaders
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
    = TableConfiguration
        { columnHeaders : ColumnHeaders msg
        , columnHeadersShown : Bool
        , rowHeaders : RowHeaders msg
        , rowHeadersShown : Bool
        , caption : Maybe (Html msg)
        , summary : Maybe (Html msg)
        , cells : List (List (Cell msg))
        , attributes : List (Html.Attribute msg)
        }


type TableError
    = NoData
    | RowLengthsDoNotMatch Int
    | ColumnHeadingMismatch
    | RowHeadingMismatch


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


simpleTable : List (List String) -> Result TableError (TableConfiguration msg)
simpleTable data =
    -- do a check that all rows have equal number of cells
    case allRowsEqualLength data of
        Ok noOfCols ->
            Ok
                (TableConfiguration
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
            Err error


allRowsEqualLength : List (List String) -> Result TableError Int
allRowsEqualLength data =
    case data of
        [] ->
            Ok 0

        row1 :: remainingRows ->
            allRowsEqualLengthHelper (List.length row1) 2 remainingRows


allRowsEqualLengthHelper : Int -> Int -> List (List a) -> Result TableError Int
allRowsEqualLengthHelper rowLength rowNumber remainingRows =
    case remainingRows of
        [] ->
            Ok rowLength

        thisRow :: rest ->
            if List.length thisRow == rowLength then
                allRowsEqualLengthHelper rowLength (rowNumber + 1) rest

            else
                Err (RowLengthsDoNotMatch rowNumber)


hideColumnHeaders : Result TableError (TableConfiguration msg) -> Result TableError (TableConfiguration msg)
hideColumnHeaders config =
    Result.map
        (\(TableConfiguration tableConfig) -> TableConfiguration { tableConfig | columnHeadersShown = False })
        config


setColumnHeadings : List String -> Result TableError (TableConfiguration msg) -> Result TableError (TableConfiguration msg)
setColumnHeadings columns resultConfig =
    --check that number equals number of columns
    resultConfig
        |> Result.map
            (\(TableConfiguration tableConfig) ->
                TableConfiguration
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
        |> Result.andThen
            (\(TableConfiguration tableConfig) ->
                if List.length columns == (List.head tableConfig.cells |> Maybe.withDefault [] |> List.length) then
                    Ok (TableConfiguration tableConfig)

                else
                    Err ColumnHeadingMismatch
            )


setRowHeadings : List String -> Result TableError (TableConfiguration msg) -> Result TableError (TableConfiguration msg)
setRowHeadings rows config =
    --check that number equals number of rows
    config
        |> Result.map
            (\(TableConfiguration tableConfig) ->
                TableConfiguration
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
        |> Result.andThen
            (\(TableConfiguration tableConfig) ->
                if List.length rows == List.length tableConfig.cells then
                    Ok (TableConfiguration tableConfig)

                else
                    Err RowHeadingMismatch
            )


hideRowHeaders : Result TableError (TableConfiguration msg) -> Result TableError (TableConfiguration msg)
hideRowHeaders config =
    Result.map
        (\(TableConfiguration tableConfig) -> TableConfiguration { tableConfig | rowHeadersShown = False })
        config


errorToString : TableError -> String
errorToString error =
    case error of
        NoData ->
            "No Data"

        RowLengthsDoNotMatch rowNumber ->
            "Every line of data should be equal in length, but row " ++ String.fromInt rowNumber ++ " has a different number of data points"

        ColumnHeadingMismatch ->
            "The number of column headings does not match the number of data points in each row. "

        RowHeadingMismatch ->
            "The number of row headings does not match the number of rows of data. "


render : Result TableError (TableConfiguration msg) -> Result TableError (Html msg)
render config =
    case config of
        Ok (TableConfiguration tableConfig) ->
            let
                spacer =
                    if tableConfig.rowHeadersShown then
                        [ td [] [] ]

                    else
                        []

                columnHeadings =
                    case tableConfig.columnHeaders of
                        ColumnHeadersSimple [] ->
                            []

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
                    case tableConfig.cells of
                        [] ->
                            []

                        _ ->
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
            case body of
                [] ->
                    Err NoData

                _ ->
                    Ok
                        (table []
                            (columnHeadings
                                ++ body
                            )
                        )

        Err error ->
            Err error
