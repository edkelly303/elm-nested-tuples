module Main exposing (..)

import App
import Browser
import Html
import Html.Events


main =
    Browser.sandbox app


type alias Model =
    { int : Int, string : String }


app =
    App.start
        { init = { int = 0, string = "" }
        , update =
            \counterRes textRes model ->
                { model
                    | int = counterRes |> Result.withDefault model.int
                    , string = textRes |> Result.withDefault model.string
                }
        }
        |> App.add counter
        |> App.add textInput
        |> App.done


type CounterMsg
    = Increment


counter =
    { init = 0
    , update =
        \msg model ->
            case msg of
                Increment ->
                    model + 1
    , view =
        \model ->
            Html.button
                [ Html.Events.onClick Increment ]
                [ Html.text (String.fromInt model) ]
    , parse = Ok
    }


type TextMsg
    = TextChanged String


textInput =
    { init = ""
    , update =
        \msg model ->
            case msg of
                TextChanged str ->
                    str
    , view =
        \model ->
            Html.input [ Html.Events.onInput TextChanged ] []
    , parse = Ok
    }
