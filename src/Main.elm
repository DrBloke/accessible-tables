module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Table
    exposing
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


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { textInput : String }


init : Model
init =
    { textInput = "" }


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    let
        table2 =
            simpleTable
                [ [ "1", "2", "3", "4", "5" ]
                , [ "1", "2", "3", "4", "5" ]
                , [ "1", "2", "3", "4", "5" ]
                ]
                --|> hideColumnHeadings
                --|> hideRowHeadings
                |> setColumnHeadings
                    (ComplexHeadings
                        [ H "One" [ "sub 1", "sub2" ]
                        , H "Two" []
                        , H "Three" [ "sub 1", "sub2" ]
                        ]
                    )
                |> setRowHeadings (Headings [ "A", "B", "C" ])
                |> render

        table1 =
            simpleTable
                [ [ "50,000", "30,000", "100,000", "80,000" ]
                , [ "10,000", "5,000", "12,000", "9,000" ]
                ]
                --|> hideColumnHeadings
                --|> hideRowHeadings
                |> setColumnHeadings
                    (ComplexHeadings
                        [ H "Mars" [ "Produced", "Sold" ]
                        , H "Venus" [ "Produced", "Sold" ]
                        ]
                    )
                |> setRowHeadings (Headings [ "Teddy Bears", "Board Games" ])
                |> render

        table3 =
            simpleTable
                [ [ "50,000", "10,000" ]
                , [ "30,000", "5,000" ]
                , [ "100,000", "12,000" ]
                , [ "80,000", "9,000" ]
                ]
                --|> hideColumnHeadings
                --|> hideRowHeadings
                |> setColumnHeadings (Headings [ "Teddy Bears", "Board Games" ])
                |> setRowHeadings
                    (ComplexHeadings
                        [ H "Mars" [ "Produced", "Sold" ]
                        , H "Venus" [ "Produced", "Sold" ]
                        ]
                    )
                |> render
    in
    div []
        [ case table1 of
            Ok table ->
                div [] [ table ]

            Err error ->
                text (errorToString error)
        , br [] []
        , case table2 of
            Ok table ->
                div [] [ table ]

            Err error ->
                text (errorToString error)
        , br [] []
        , case table3 of
            Ok table ->
                div [] [ table ]

            Err error ->
                text (errorToString error)
        ]
