module Main exposing (..)

import Browser
import Composer
import Html
import Html.Events


main : Program () ComponentsModel ComponentsMsg
main =
    Browser.sandbox components


type alias ComponentsModel =
    ( Int, ( String, () ) )


type alias ComponentsMsg =
    ( Composer.Msg CounterMsg, ( Composer.Msg TextMsg, () ) )


components =
    Composer.start
        |> Composer.add counter
        |> Composer.add textInput
        |> Composer.end


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
            Html.div
                []
                [ Html.input [ Html.Events.onInput TextChanged ] [ Html.text model ]
                , Html.text model
                ]
    }
