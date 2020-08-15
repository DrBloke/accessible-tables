module Table exposing
    ( ComplexHeading(..)
    , Headings(..)
    , TableError(..)
    , errorToString
    , hideColumnHeadings
    , hideRowHeadings
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
        { columnHeadings : ColumnHeadings msg
        , columnHeadingsShown : Bool
        , rowHeadings : RowHeadings msg
        , rowHeadingsShown : Bool
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


type ColumnHeadings msg
    = ColumnHeadingsSimple (List (ColumnHeadingSingle msg))
    | ColumnHeadingsComplex (List (ColumnHeadingGroup msg))


type ColumnHeadingSingle msg
    = ColumnHeadingSingle (ColumnHeading msg)


type ColumnHeadingGroup msg
    = ColumnHeadingGroup (ColumnHeading msg) (List (ColumnHeading msg))


type ColumnHeading msg
    = ColumnHeading
        { label : Html msg
        , attributes : List (Html.Attribute msg)
        }


type RowHeadings msg
    = RowHeadingsSimple (List (RowHeadingSingle msg))
    | RowHeadingsComplex (List (RowHeadingGroup msg))


type RowHeadingSingle msg
    = RowHeadingSingle (RowHeading msg)


type RowHeadingGroup msg
    = RowHeadingGroup (RowHeading msg) (List (RowHeading msg))


type RowHeading msg
    = RowHeading
        { label : Html msg
        , attributes : List (Html.Attribute msg)
        }


type Cell msg
    = Cell
        { value : Html msg
        , attributes : List (Html.Attribute msg)
        }


type Headings
    = Headings (List String)
    | ComplexHeadings (List ComplexHeading)


type ComplexHeading
    = H String (List String)


simpleTable : List (List String) -> Result TableError (TableConfiguration msg)
simpleTable data =
    -- do a check that all rows have equal number of cells
    case allRowsEqualLength data of
        Ok noOfCols ->
            Ok
                (TableConfiguration
                    { columnHeadings =
                        ColumnHeadingsSimple
                            (List.range 1 noOfCols
                                |> List.map
                                    (\colNo ->
                                        ColumnHeadingSingle
                                            (ColumnHeading
                                                { label = text <| "Column " ++ String.fromInt colNo
                                                , attributes = []
                                                }
                                            )
                                    )
                            )
                    , columnHeadingsShown = True
                    , rowHeadings =
                        RowHeadingsSimple
                            (List.range 1 (List.length data)
                                |> List.map
                                    (\rowNo ->
                                        RowHeadingSingle (RowHeading { label = text <| "Row " ++ String.fromInt rowNo, attributes = [] })
                                    )
                            )
                    , rowHeadingsShown = True
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


hideColumnHeadings : Result TableError (TableConfiguration msg) -> Result TableError (TableConfiguration msg)
hideColumnHeadings config =
    Result.map
        (\(TableConfiguration tableConfig) -> TableConfiguration { tableConfig | columnHeadingsShown = False })
        config


setColumnHeadings : Headings -> Result TableError (TableConfiguration msg) -> Result TableError (TableConfiguration msg)
setColumnHeadings headings_ resultConfig =
    case headings_ of
        Headings headings ->
            resultConfig
                |> Result.map
                    (\(TableConfiguration tableConfig) ->
                        TableConfiguration
                            { tableConfig
                                | columnHeadings =
                                    ColumnHeadingsSimple
                                        (List.map
                                            (\label ->
                                                ColumnHeadingSingle
                                                    (ColumnHeading
                                                        { label = Html.text label
                                                        , attributes = []
                                                        }
                                                    )
                                            )
                                            headings
                                        )
                            }
                    )
                --check that number equals number of headings
                |> Result.andThen
                    (\(TableConfiguration tableConfig) ->
                        if List.length headings == (List.head tableConfig.cells |> Maybe.withDefault [] |> List.length) then
                            Ok (TableConfiguration tableConfig)

                        else
                            Err ColumnHeadingMismatch
                    )

        ComplexHeadings complexHeadings ->
            resultConfig
                |> Result.map
                    (\(TableConfiguration tableConfig) ->
                        TableConfiguration
                            { tableConfig
                                | columnHeadings =
                                    ColumnHeadingsComplex
                                        (List.map
                                            (\(H mainHeading subHeadings) ->
                                                ColumnHeadingGroup
                                                    (ColumnHeading
                                                        { label = Html.text mainHeading
                                                        , attributes = []
                                                        }
                                                    )
                                                    (List.map
                                                        (\subHeading ->
                                                            ColumnHeading
                                                                { label = Html.text subHeading
                                                                , attributes = []
                                                                }
                                                        )
                                                        subHeadings
                                                    )
                                            )
                                            complexHeadings
                                        )
                            }
                    )
                --check that number equals number of headings
                |> Result.andThen
                    (\(TableConfiguration tableConfig) ->
                        if noOfComplexColumnHeadings complexHeadings == (List.head tableConfig.cells |> Maybe.withDefault [] |> List.length) then
                            Ok (TableConfiguration tableConfig)

                        else
                            Err ColumnHeadingMismatch
                    )


noOfComplexColumnHeadings : List ComplexHeading -> Int
noOfComplexColumnHeadings complexHeadings =
    let
        addHeaders : ComplexHeading -> Int -> Int
        addHeaders header acc =
            case header of
                H _ [] ->
                    acc + 1

                H _ subHeadings ->
                    acc + List.length subHeadings
    in
    List.foldl addHeaders 0 complexHeadings


setRowHeadings : List String -> Result TableError (TableConfiguration msg) -> Result TableError (TableConfiguration msg)
setRowHeadings rows config =
    --check that number equals number of rows
    config
        |> Result.map
            (\(TableConfiguration tableConfig) ->
                TableConfiguration
                    { tableConfig
                        | rowHeadings =
                            RowHeadingsSimple
                                (List.map
                                    (\label ->
                                        RowHeadingSingle
                                            (RowHeading
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


hideRowHeadings : Result TableError (TableConfiguration msg) -> Result TableError (TableConfiguration msg)
hideRowHeadings config =
    Result.map
        (\(TableConfiguration tableConfig) -> TableConfiguration { tableConfig | rowHeadingsShown = False })
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
                columnHeadings =
                    case tableConfig.columnHeadings of
                        ColumnHeadingsSimple [] ->
                            []

                        ColumnHeadingsSimple cols ->
                            let
                                spacer =
                                    if tableConfig.rowHeadingsShown then
                                        [ td [] [] ]

                                    else
                                        []
                            in
                            [ thead []
                                [ tr [ hidden (not tableConfig.columnHeadingsShown) ]
                                    (spacer
                                        ++ List.map
                                            (\(ColumnHeadingSingle (ColumnHeading colInfo)) ->
                                                th [ scope "col" ] [ colInfo.label ]
                                            )
                                            cols
                                    )
                                ]
                            ]

                        ColumnHeadingsComplex [] ->
                            []

                        ColumnHeadingsComplex complexHeadings ->
                            let
                                colStructure =
                                    let
                                        colSpacer =
                                            if tableConfig.rowHeadingsShown then
                                                [ col [] [] ]

                                            else
                                                []

                                        span_ n =
                                            if n == 0 then
                                                []

                                            else
                                                [ attribute "span" (String.fromInt n) ]
                                    in
                                    colgroup []
                                        (colSpacer
                                            ++ List.map
                                                (\(ColumnHeadingGroup (ColumnHeading colInfo) subHeads) ->
                                                    col (span_ (List.length subHeads)) []
                                                )
                                                complexHeadings
                                        )

                                spacer =
                                    if tableConfig.rowHeadingsShown then
                                        [ td [] [] ]

                                    else
                                        []

                                mainHeadings : List (Html msg)
                                mainHeadings =
                                    let
                                        colspan_ n =
                                            if n == 0 then
                                                []

                                            else
                                                [ colspan n ]

                                        rowspan_ n =
                                            if n == 0 then
                                                [ rowspan 2 ]

                                            else
                                                []
                                    in
                                    List.map
                                        (\(ColumnHeadingGroup (ColumnHeading colInfo) subHeads) ->
                                            th ([ scope "col" ] ++ colspan_ (List.length subHeads) ++ rowspan_ (List.length subHeads)) [ colInfo.label ]
                                        )
                                        complexHeadings

                                subHeadings : List (Html msg)
                                subHeadings =
                                    List.map
                                        (\(ColumnHeadingGroup (ColumnHeading _) subHeads) ->
                                            List.map
                                                (\(ColumnHeading colInfo) ->
                                                    th [ scope "col" ] [ colInfo.label ]
                                                )
                                                subHeads
                                        )
                                        complexHeadings
                                        |> List.concat
                            in
                            [ colStructure
                            , thead []
                                [ tr [ hidden (not tableConfig.columnHeadingsShown) ]
                                    (spacer ++ mainHeadings)
                                , tr [ hidden (not tableConfig.columnHeadingsShown) ]
                                    (spacer ++ subHeadings)
                                ]
                            ]

                body =
                    case tableConfig.cells of
                        [] ->
                            []

                        _ ->
                            case tableConfig.rowHeadings of
                                RowHeadingsSimple rowHeadings ->
                                    [ tbody []
                                        (List.zip tableConfig.cells rowHeadings
                                            |> List.map
                                                (\( rowDataInfo, RowHeadingSingle (RowHeading rowHeadingInfo) ) ->
                                                    tr [] ([ th ([ scope "row", hidden (not tableConfig.rowHeadingsShown) ] ++ rowHeadingInfo.attributes) [ rowHeadingInfo.label ] ] ++ List.map (\(Cell cell) -> td cell.attributes [ cell.value ]) rowDataInfo)
                                                )
                                        )
                                    ]

                                RowHeadingsComplex _ ->
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
