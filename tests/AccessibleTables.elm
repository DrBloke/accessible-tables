module AccessibleTables exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html exposing (..)
import Html.Attributes exposing (..)
import Table exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Accessible Table Module"
        [ describe "Table with two tier headers"
            [ test "Can create W3.org Example 1" <|
                \_ ->
                    let
                        generatedTable =
                            simpleTable
                                [ [ "50,000", "30,000", "100,000", "80,000" ]
                                , [ "10,000", "5,000", "12,000", "9,000" ]
                                ]
                                |> setColumnHeadings
                                    (ComplexHeadings
                                        [ H "Mars" [ "Produced", "Sold" ]
                                        , H "Venus" [ "Produced", "Sold" ]
                                        ]
                                    )
                                |> setRowHeadings [ "Teddy Bears", "Board Games" ]
                                |> render
                                |> Result.withDefault (text "failed to generate")

                        w3example =
                            table []
                                [ col [] []
                                , colgroup [ Html.Attributes.attribute "span" "2" ]
                                    []
                                , colgroup [ Html.Attributes.attribute "span" "2" ]
                                    []
                                , thead []
                                    [ tr []
                                        [ td [ rowspan 2 ]
                                            []
                                        , th [ colspan 2, scope "colgroup" ]
                                            [ text "Mars" ]
                                        , th [ colspan 2, scope "colgroup" ]
                                            [ text "Venus" ]
                                        ]
                                    , tr []
                                        [ th [ scope "col" ]
                                            [ text "Produced" ]
                                        , th [ scope "col" ]
                                            [ text "Sold" ]
                                        , th [ scope "col" ]
                                            [ text "Produced" ]
                                        , th [ scope "col" ]
                                            [ text "Sold" ]
                                        ]
                                    ]
                                , tbody []
                                    [ tr []
                                        [ th [ scope "row" ]
                                            [ text "Teddy Bears" ]
                                        , td []
                                            [ text "50,000" ]
                                        , td []
                                            [ text "30,000" ]
                                        , td []
                                            [ text "100,000" ]
                                        , td []
                                            [ text "80,000" ]
                                        ]
                                    , tr []
                                        [ th [ scope "row" ]
                                            [ text "Board Games" ]
                                        , td []
                                            [ text "10,000" ]
                                        , td []
                                            [ text "5,000" ]
                                        , td []
                                            [ text "12,000" ]
                                        , td []
                                            [ text "9,000" ]
                                        ]
                                    ]
                                ]
                    in
                    Expect.equal generatedTable w3example
            ]
        ]
