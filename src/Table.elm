module Table exposing (render, simpleTable)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


type TableConfiguration msg
    = ValidTable (Table msg)
    | MalformedTable String


type Table msg
    = Table
        { columnnHeaders : List (ColumnHeaders msg)
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
    --do a check for noOfColumns same in every row and equal to no of column headings
    ValidTable
        (Table
            { columnnHeaders =
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


render : TableConfiguration msg -> Html msg
render config =
    case config of
        ValidTable (Table tableConfig) ->
            table []
                (List.map
                    (\row ->
                        tr [] (List.map (\(Cell cell) -> td [] [ cell.value ]) row)
                    )
                    tableConfig.cells
                )

        MalformedTable error ->
            text error



--
-- setColumnHeadings : List String -> TableConfiguration data msg -> TableConfiguration data msg ->
-- setColumnHeadings columns config =
--   case config of
--       ValidTable (Table tableConfig) ->
--           case tableConfig.columnns of
--
--
--       MalformedTable error ->
--           text error
