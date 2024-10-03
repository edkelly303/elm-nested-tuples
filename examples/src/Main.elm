module Main exposing (..)

import App
import Browser
import Html
import Html.Attributes
import Html.Events


main =
    Browser.sandbox app


type alias Model =
    { bool : Bool }


type Msg
    = CheckboxClicked Bool


app =
    App.start
        { init = { bool = False }
        , update =
            \counterValue floatFieldValue maybeMsg model ->
                if counterValue > 5 || floatFieldValue == Just 0.0 then
                    { model | bool = False }

                else
                    case maybeMsg of
                        Just (CheckboxClicked bool) ->
                            { model
                                | bool =
                                    if counterValue > 5 || floatFieldValue == Just 0.0 then
                                        False

                                    else
                                        bool
                            }

                        Nothing ->
                            model
        , view =
            \counter_ floatField_ toMsg model ->
                Html.div []
                    [ Html.p []
                        [ Html.h4 [] [ Html.text "These two views come from encapsulated components, which manage their own state" ]
                        , Html.div [] [ counter_.view ]
                        , Html.div [] [ floatField_.view ]
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
                        , Html.small [] [ Html.text "(We have set some rules in our update function: the checkbox will always be false if the counter is > 5 or the text box contains the float value \"0.0\")" ]
                        ]
                    , Html.p []
                        [ Html.h4 [] [ Html.text "Here we take a peek at the values of the components directly" ]
                        , Html.div [] [ Html.text (Debug.toString counter_.value) ]
                        , Html.div [] [ Html.text (Debug.toString floatField_.value) ]
                        ]
                    , Html.p
                        []
                        [ Html.h4 [] [ Html.text "Here we send a message from the user's view to one of the components" ]
                        , Html.button
                            [ Html.Events.onClick (counter_.send Reset) ]
                            [ Html.text "Reset counter" ]
                        ]
                    , Html.p []
                        [ Html.h4 [] [ Html.text "And here's a look at our app's model - gloriously unpolluted by component state" ]
                        , Html.text (Debug.toString model)
                        ]
                    ]
        }
        |> App.add counter
        |> App.add floatField
        |> App.done


type CounterMsg
    = Increment
    | Reset


counter =
    { init = 0
    , update =
        \msg model ->
            case msg of
                Increment ->
                    model + 1

                Reset ->
                    0
    , view =
        \model ->
            Html.button
                [ Html.Events.onClick Increment ]
                [ Html.text (String.fromInt model) ]
    , parse =
        identity
    }


type FloatMsg
    = FloatChanged String


floatField =
    { init = ""
    , update =
        \msg model ->
            case msg of
                FloatChanged str ->
                    str
    , view =
        \model ->
            Html.input [ Html.Events.onInput FloatChanged ] []
    , parse =
        String.toFloat
    }
