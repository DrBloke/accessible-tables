module Main exposing (table)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


type Table
    = Table
        { columnHeaders : Maybe (List ColumnHeaderGroup)
        , rowHeaders : Maybe (List (List RowHeaderGroup))
        , caption : Maybe Caption
        , summary : Maybe Summary
        , cells : List Cells
        }


type ColumnHeader msg
    = ColumnHeader
        { offset : Int
        , label : data -> Html msg
        , span : Int
        }


type RowHeader msg
    = RowHeader
        { offset : Int
        , label : data -> Html msg
        , span : Int
        }


type ColumnHeaderGroup
    = ColumnHeaderSingle ColumnHeader
    | ColumnHeaderGroup (List ColumnHeader)


type RowHeaderGroup
    = RowHeaderSingle RowHeader
    | RowHeaderGroup (List RowHeader)
