module Table exposing
    ( columHeadersNotShown
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
        { columnHeaders : List (ColumnHeaders msg)
        , columnHeadersShown : Bool
        , rowHeaders : List (RowHeaders msg)
        , rowHeadersShown : Bool
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
    -- do a check that all rows have equal number of cells
    case allRowsEqualLength data of
        Ok noOfCols ->
            ValidTable
                (Table
                    { columnHeaders = List.range 1 noOfCols |> List.map (\colNo -> ColumnHeaderSingle (ColumnHeader { label = text (String.fromInt colNo), attributes = [] }))
                    , columnHeadersShown = True
                    , rowHeaders = List.range 1 (List.length data) |> List.map (\rowNo -> RowHeaderSingle (RowHeader { label = text (String.fromInt rowNo), attributes = [] }))
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

        [ [] ] ->
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


columHeadersNotShown : TableConfiguration msg -> TableConfiguration msg
columHeadersNotShown config =
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



--
-- setRowHeadings : List String -> TableConfiguration msg -> TableConfiguration msg
-- setRowHeadings rows config =
--     --check that number equals number of rows
--     case config of
--         ValidTable (Table tableConfig) ->
--             ValidTable
--                 (Table
--                     { tableConfig
--                         | rowHeaders =
--                             Just
--                                 (List.map
--                                     (\label ->
--                                         RowHeaderSingle
--                                             (RowHeader
--                                                 { label = Html.text label
--                                                 , attributes = []
--                                                 }
--                                             )
--                                     )
--                                     rows
--                                 )
--                     }
--                 )
--
--         MalformedTable error ->
--             MalformedTable error
--
-- validateColumns : TableConfiguration msg -> TableConfiguration msg
-- validateColumns config =
--     case config of
--         MalformedTable error ->
--             MalformedTable error
--
--         ValidTable (Table validConfig) ->
--             case validConfig.columnHeaders of
--                 Nothing ->
--                     if validConfig.columnHeadersShown then
--                         MalformedTable "Column headers are Shown"
--
--                     else
--                         let
--                             noOfColumns =
--                                 case List.head validConfig.cells of
--                                     Nothing ->
--                                         0
--
--                                     Just row ->
--                                         List.length row
--                         in
--                         ValidTable
--                             (Table
--                                 { validConfig
--                                     | columnHeaders =
--                                         Just
--                                             (List.repeat noOfColumns
--                                                 (ColumnHeaderSingle
--                                                     (ColumnHeader
--                                                         { label = text ""
--                                                         , attributes = []
--                                                         }
--                                                     )
--                                                 )
--                                             )
--                                 }
--                             )
--
--                 Just _ ->
--                     ValidTable (Table validConfig)


render : TableConfiguration msg -> Html msg
render config =
    case config of
        ValidTable (Table tableConfig) ->
            case tableConfig.cells of
                [ [] ] ->
                    text "no data"

                _ ->
                    let
                        columnHeadings =
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
