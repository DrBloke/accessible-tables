module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Table as Table
    exposing
        ( ComplexHeading(..)
        , Headings(..)
        , TableError(..)
        , errorToString
        , hideColumnHeadings
        , hideRowHeadings
        , view
        , setColumnHeadings
        , setRowHeadings
        , generate
        )


css : String
css =
    """
html {
        font-family: sans-serif;
    }

table {
    border-collapse: collapse;
    border: 2px solid rgb(200, 200, 200);
    letter-spacing: 1px;
    font-size: 0.8rem;
}

td,
th {
    border: 1px solid rgb(190, 190, 190);
    padding: 10px 20px;
}

th {
    background-color: rgb(235, 235, 235);
}

td {
    text-align: center;
}

tr:nth-child(even) td {
    background-color: rgb(250, 250, 250);
}

tr:nth-child(odd) td{
    background-color: rgb(245, 245, 245);
}

caption {
    padding: 10px;
}

tbody tr:hover td, tr:hover th[scope="row"] {
  background: yellow;
}
    """


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
            generate
                [ [ "1", "2", "3", "4", "5" ]
                , [ "1", "2", "3", "4", "5" ]
                , [ "1", "2", "3", "4", "5" ]
                ]
                |> setColumnHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "One" [ "sub 1", "sub2" ]
                        , HeadingAndSubHeadings "Two" []
                        , HeadingAndSubHeadings "Three" [ "sub 1", "sub2" ]
                        ]
                    )
                |> setRowHeadings (Headings [ "A", "B", "C" ])
                |> Table.view

        table1 =
            generate
                [ [ "50,000", "30,000", "100,000", "80,000" ]
                , [ "10,000", "5,000", "12,000", "9,000" ]
                ]
                |> setColumnHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "Mars" [ "Produced", "Sold" ]
                        , HeadingAndSubHeadings "Venus" [ "Produced", "Sold" ]
                        ]
                    )
                |> setRowHeadings (Headings [ "Teddy Bears", "Board Games" ])
                |> Table.view

        table3 =
            generate
                [ [ "50,000", "10,000" ]
                , [ "30,000", "5,000" ]
                , [ "100,000", "12,000" ]
                , [ "80,000", "9,000" ]
                ]
                |> setColumnHeadings (Headings [ "Teddy Bears", "Board Games" ])
                |> setRowHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "Mars" [ "Produced", "Sold" ]
                        , HeadingAndSubHeadings "Venus" [ "Produced", "Sold" ]
                        ]
                    )
                |> Table.view
    in
    Html.div []
        [ Html.node "style" [] [ Html.text css ]
        , div []
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
        ]
