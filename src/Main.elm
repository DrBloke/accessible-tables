module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Table exposing (columHeadersNotShown, render, setColumnHeadings, simpleTable)


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
    div []
        [ simpleTable
            [ [ "1", "2", "3" ]
            , [ "4", "5", "6" ]
            , [ "7", "8", "9" ]
            ]
            --|> columHeadersNotRequired
            --|> setColumnHeadings [ "one", "two", "three" ]
            |> render
        ]
