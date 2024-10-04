module Main exposing (..)

import App
import Browser
import Html
import Html.Attributes
import Html.Events
import Process
import Task
import Time


main :
    Program
        ()
        ( { timerExpired : Bool }, ( Maybe Int, () ) )
        ( Maybe Msg, ( Maybe TimerMsg, () ) )
main =
    Browser.element app


type alias Model =
    { timerExpired : Bool }


type Msg
    = TimerExpired


app =
    App.start
        { init = { timerExpired = False }
        , update =
            \msg model ->
                case msg of
                    TimerExpired ->
                        ( { timerExpired = True }, Cmd.none )
        , view =
            \counterButton_ sendToSelf model ->
                Html.div []
                    [ Html.p []
                        [ Html.h4 [] [ Html.text "The timer element below comes from an encapsulated component, which manages its own state and can send messages to the app's update function" ]
                        , Html.div [] [ counterButton_.view ]
                        ]
                    , Html.p
                        []
                        [ Html.h4 [] [ Html.text "Here we send a message from the app's view to the component" ]
                        , Html.button
                            [ Html.Events.onClick (counterButton_.send Reset) ]
                            [ Html.text "Reset timer" ]
                        ]
                    , Html.p []
                        [ Html.h4 [] [ Html.text "And here's a look at our app's model - gloriously unpolluted by component state" ]
                        , Html.text (Debug.toString model)
                        ]
                    ]
        , subscriptions = \model -> Sub.none
        }
        |> App.add (timer { timerExpired = TimerExpired })
        |> App.done


type TimerMsg
    = Start
    | Tick
    | Reset


timer { timerExpired } =
    { init = Nothing
    , update =
        \cmd msg model ->
            case msg of
                Start ->
                    ( Just 10, Cmd.none )

                Tick ->
                    if model == Just 0 then
                        ( Nothing, cmd.toParent (Task.perform (\_ -> timerExpired) (Process.sleep 0)) )

                    else
                        ( Maybe.map (\n -> n - 1) model, Cmd.none )

                Reset ->
                    ( Nothing, cmd.toParent (Task.perform (\_ -> timerExpired) (Process.sleep 0)) )
    , view =
        \model ->
            Html.article []
                [ Html.strong [] [ Html.text "Countdown timer" ]
                , Html.h2 [] [ Html.text (model |> Maybe.withDefault 10 |> String.fromInt) ]
                , Html.button
                    [ Html.Events.onClick Start ]
                    [ Html.text "Start" ]
                , Html.button
                    [ Html.Events.onClick Reset ]
                    [ Html.text "Reset" ]
                ]
    , subscriptions =
        \model ->
            case model of
                Nothing ->
                    Sub.none

                Just _ ->
                    Time.every 1000 (\_ -> Tick)
    }


type FloatMsg
    = FloatChanged String


floatInput =
    { init = ""
    , update =
        \cmd msg model ->
            case msg of
                FloatChanged str ->
                    ( str, Cmd.none )
    , view =
        \model ->
            Html.input [ Html.Events.onInput FloatChanged ] []
    , subscriptions =
        \model -> Sub.none
    }
