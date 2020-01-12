module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { textInput : String }


init : Model
init =
    { textInput = "" }


type Msg
    = ChangeInput String
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeInput inputText ->
            { model | textInput = inputText }

        NoOp ->
            model


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Enter formula", onInput ChangeInput ] []
        , br [] []
        , text model.textInput
        ]
