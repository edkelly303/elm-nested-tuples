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
    | TimerReset


app =
    App.start
        { init = { timerExpired = False }
        , update =
            \msg model ->
                case msg of
                    TimerExpired ->
                        ( { model | timerExpired = True }, Cmd.none )

                    TimerReset ->
                        ( { model | timerExpired = False }, Cmd.none )
        , view =
            \timer_ sendToSelf model ->
                Html.div []
                    [ Html.p []
                        [ Html.text
                            """
                            The timer element below comes from an 
                            encapsulated component, which manages its own 
                            state, completely separately from the app's 
                            model. It can send messages to the app's update 
                            function, and receive messages sent by the app's 
                            view function.
                            """
                        ]
                    , timer_.view
                    , Html.p []
                        [ Html.text
                            """
                            Here is a `Debug.toString` of the app's model:
                            """
                        ]
                    , Html.p [ Html.Attributes.style "font-family" "monospace" ]
                        [ Html.text (Debug.toString model) ]
                    , Html.p []
                        [ Html.text
                            """
                            The only thing the app's model does is keep track of
                            whether the timer has expired. But as you can see, 
                            it doesn't (and can't) know anything about the 
                            actual state of the timer - whether it's ticking, 
                            how much time is left, etc. The only way it can find 
                            out whether the timer has expired is if it receives 
                            a message from the timer component. So, the timer's
                            update function is configured to send a message to
                            the app's update function when it reaches zero
                            """
                        ]

                    , Html.p []
                        [ Html.text
                            """
                            Here we have a button in the app's view function 
                            that can send a message to the timer component
                            """
                        ]
                    , Html.button
                        [ Html.Events.onClick (timer_.send Reset) ]
                        [ Html.text "Reset timer" ]
                    ]
        , subscriptions = \model -> Sub.none
        }
        |> App.add (timer { timerExpired = TimerExpired, timerReset = TimerReset })
        |> App.done


type TimerMsg
    = Start
    | Tick
    | Reset


timer { timerExpired, timerReset } =
    { init = Nothing
    , update =
        \cmd msg model ->
            case msg of
                Start ->
                    ( Just 10, Cmd.none )

                Tick ->
                    if model == Just 0 then
                        ( Just 0, cmd.toParent (Task.perform (\_ -> timerExpired) (Process.sleep 0)) )

                    else
                        ( Maybe.map (\n -> n - 1) model, Cmd.none )

                Reset ->
                    ( Nothing, cmd.toParent (Task.perform (\_ -> timerReset) (Process.sleep 0)) )
    , view =
        \model ->
            Html.article
                [ Html.Attributes.style "border" "solid 1px pink"
                , Html.Attributes.style "border-radius" "10px"
                , Html.Attributes.style "background-color" "aliceblue"
                , Html.Attributes.style "padding" "10px"
                , Html.Attributes.style "width" "200px"
                , Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "font-family" "sans-serif"
                ]
                [ Html.p [] [ Html.text "Countdown timer" ]
                , Html.h1 [] [ Html.text (model |> Maybe.withDefault 10 |> String.fromInt) ]
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
