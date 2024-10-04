module Main exposing (..)

import App
import Browser
import Html
import Html.Attributes
import Html.Events
import Process
import Task


main :
    Program
        ()
        ( { bool : Bool }, ( Int, ( String, () ) ) )
        ( Maybe Msg, ( App.Msg CounterMsg, ( App.Msg FloatMsg, () ) ) )
main =
    Browser.element app


type alias Model =
    { bool : Bool }


type Msg
    = CheckboxClicked Bool
    | CounterButtonComponentReset


app =
    App.start
        { init = { bool = False }
        , update =
            \msg model ->
                case msg of
                    CheckboxClicked bool ->
                        ( { model | bool = bool }, Cmd.none )

                    CounterButtonComponentReset ->
                        ( Debug.log "Counter component has been reset!" model, Cmd.none )
        , view =
            \counterButton_ floatInput_ toMsg model ->
                Html.div []
                    [ Html.p []
                        [ Html.h4 [] [ Html.text "These two views come from encapsulated components, which manage their own state" ]
                        , Html.div [] [ counterButton_.view ]
                        , Html.div [] [ floatInput_.view ]
                        ]
                    , Html.p []
                        [ Html.h4 [] [ Html.text "By contrast, the state of this checkbox lives in the user's model" ]
                        , Html.div []
                            [ Html.input
                                [ Html.Attributes.type_ "checkbox"
                                , Html.Attributes.checked model.bool
                                , Html.Events.onCheck (toMsg << CheckboxClicked)
                                ]
                                []
                            ]
                        , Html.small [] [ Html.text "(We have set some rules in our update function: the checkbox will always be false if the counterButton is > 5 or the text box contains the float value \"0.0\")" ]
                        ]
                    , Html.p
                        []
                        [ Html.h4 [] [ Html.text "Here we send a message from the user's view to one of the components" ]
                        , Html.button
                            [ Html.Events.onClick (counterButton_.send Reset) ]
                            [ Html.text "Reset counterButton" ]
                        ]
                    , Html.p []
                        [ Html.h4 [] [ Html.text "And here's a look at our app's model - gloriously unpolluted by component state" ]
                        , Html.text (Debug.toString model)
                        ]
                    ]
        }
        |> App.add (counterButton { notifyParent = CounterButtonComponentReset })
        |> App.add floatInput
        |> App.done


type CounterMsg
    = Increment
    | Reset


counterButton { notifyParent } =
    { init = 0
    , update =
        \cmd msg model ->
            case msg of
                Increment ->
                    ( model + 1, cmd.toSelf (Task.perform (\_ -> Reset) (Process.sleep 10000)) )

                Reset ->
                    ( 0, cmd.toParent (Task.perform (\_ -> notifyParent) (Process.sleep 0)) )
    , view =
        \model ->
            Html.button
                [ Html.Events.onClick Increment ]
                [ Html.text (String.fromInt model) ]
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
    }
