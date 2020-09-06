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
        table1 =
            generate
                [ [ "1", "2" ]
                , [ "3", "4" ] ]
                |> Table.view      
        table2 =
            generate
                [ [ "1", "2" ]
                , [ "3", "4" ] ]
                |> hideRowHeadings
                |> Table.view      

        table3 =
            generate
                [ [ "1", "2" ]
                , [ "3", "4" ] ]
                |> hideColumnHeadings
                |> Table.view      

        table4 =
            generate
                [ [ "1", "2" ]
                , [ "3", "4" ] ]
                |> setColumnHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "One" [ "sub 1", "sub2" ] ]
                    )
                |> Table.view      
        table5 =
            generate
                [ [ "1", "2" ]
                , [ "3", "4" ] ]
                |> setColumnHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "One" [ "sub 1", "sub2" ] ]
                    )
                |> hideRowHeadings
                |> Table.view    
        table6 =
            generate
                [ [ "1", "2" ]
                , [ "3", "4" ] ]
                |> setRowHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "One" [ "sub 1", "sub 2" ]
                        ]
                    )
                |> Table.view    
        table7 =
            generate
                [ [ "1", "2" ]
                , [ "3", "4" ] ]
                |> setRowHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "One" [ "sub 1", "sub 2" ]
                        ]
                    )
                |> hideColumnHeadings
                |> Table.view    
        table8 =
            generate
                [ [ "1", "2" ]
                , [ "3", "4" ] ]
                |> setRowHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "One" [ "sub 1", "sub 2" ]
                        ]
                    )
                |> setColumnHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "Row One" [ "Row sub 1", "Row sub2" ] ]
                    )
                |> Table.view    
        table9 =
            generate
                [ [ "1", "2" ] ]
                |> setColumnHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "One" [ "sub 1", "sub 2" ]
                        ]
                    )
                |> setRowHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "Row One" [ "Row sub 1"] ]
                    )
                |> Table.view    
        table10 =
            generate
                [ [ "1", "2" ] ]
                |> setColumnHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "One" [ "sub 1", "sub 2" ]
                        ]
                    )
                |> setRowHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "Row One" [ ] ]
                    )
                |> Table.view    
        table11 =
            generate
                [ [ "1", "2" ]
                , [ "3", "4" ] ]
                |> setColumnHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "One" [ "sub 1", "sub 2" ]
                        ]
                    )
                |> setRowHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "Row One" [ ]
                        , HeadingAndSubHeadings "Row Two" ["sub row 1" ] ]
                    )
                |> Table.view    
        table12 =
            generate
                [ [ "1", "2" ]
                , [ "3", "4" ]
                , [ "5", "6" ] ]
                |> setColumnHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "One" [ "sub 1", "sub 2" ]
                        ]
                    )
                |> setRowHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "Row One" [ ]
                        , HeadingAndSubHeadings "Row Two" ["sub row 1", "sub row 2"] ]
                    )
                |> Table.view    
        table13 =
            generate
                [ [ "1", "2", "3", "4" ]
                , [ "5", "6", "7", "8" ]
                , [ "9", "10", "11", "12"] ]
                |> setColumnHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "One" [ "sub 1", "sub 2" ]
                        , HeadingAndSubHeadings "Two" [ "sub 3", "sub 4" ]
                        ]
                    )
                |> setRowHeadings
                    (ComplexHeadings
                        [ HeadingAndSubHeadings "Row One" [ ]
                        , HeadingAndSubHeadings "Row Two" ["sub row 1", "sub row 2"] ]
                    )
                |> Table.view    
        table20 =
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

        table21 =
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

        table22 =
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
            , br [][]  
            , case table2 of
                Ok table ->
                    div [] [ table ]

                Err error ->
                    text (errorToString error)
            , br [][]  
            , case table3 of
                Ok table ->
                    div [] [ table ]

                Err error ->
                    text (errorToString error)
            , br [][]  
            , case table4 of
                Ok table ->
                    div [] [ table ]

                Err error ->
                    text (errorToString error)
            , br [][]  
            , case table5 of
                Ok table ->
                    div [] [ table ]

                Err error ->
                    text (errorToString error)
            , br [][]  
            , case table6 of
                Ok table ->
                    div [] [ table ]

                Err error ->
                    text (errorToString error)
            , br [][]  
            , case table7 of
                Ok table ->
                    div [] [ table ]

                Err error ->
                    text (errorToString error)
            , br [][]  
            , case table8 of
                Ok table ->
                    div [] [ table ]

                Err error ->
                    text (errorToString error)
            , br [][]  
            , case table9 of
                Ok table ->
                    div [] [ table ]

                Err error ->
                    text (errorToString error)
            , br [][]  
            , case table10 of
                Ok table ->
                    div [] [ table ]

                Err error ->
                    text (errorToString error)
            , br [][]  
            , case table11 of
                Ok table ->
                    div [] [ table ]

                Err error ->
                    text (errorToString error)
            , br [][]  
            , case table12 of
                Ok table ->
                    div [] [ table ]

                Err error ->
                    text (errorToString error)
            , br [][]  
            , case table13 of
                Ok table ->
                    div [] [ table ]

                Err error ->
                    text (errorToString error)
            , br [][]  
            , case table20 of
                Ok table ->
                    div [] [ table ]

                Err error ->
                    text (errorToString error)
            , br [] []
            , case table21 of
                Ok table ->
                    div [] [ table ]

                Err error ->
                    text (errorToString error)
            , br [] []
            , case table22 of
                Ok table ->
                    div [] [ table ]

                Err error ->
                    text (errorToString error)
            , br [] []
            ]
        ]
