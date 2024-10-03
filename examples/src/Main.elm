module Main exposing (..)

import App
import Browser
import Html
import Html.Attributes
import Html.Events


main =
    Browser.sandbox app


type alias Model =
    { int : Int, float : Float, bool : Bool }


type Msg
    = CheckboxClicked


app =
    App.start
        { init = { int = 0, float = 0.0, bool = False }
        , update =
            \counterOutput floatFieldOutput maybeMsg model ->
                -- update user's model with output from components
                let
                    modelUpdatedFromComponents =
                        { model
                            | int = counterOutput
                            , float = floatFieldOutput |> Maybe.withDefault model.float
                        }
                in
                -- handle user's own msgs for model updates
                case maybeMsg of
                    Just CheckboxClicked ->
                        { modelUpdatedFromComponents | bool = not model.bool }

                    Nothing ->
                        modelUpdatedFromComponents
        , view =
            \counterView floatFieldView toMsg model ->
                Html.div []
                    -- these two are encapsulated components
                    [ counterView
                    , floatFieldView

                    -- this one's state lives in the user's model
                    , Html.input
                        [ Html.Attributes.type_ "checkbox"
                        , Html.Attributes.checked bool
                        , Html.Events.onCheck (\_ -> toMsg CheckboxClicked)
                        ]
                        []
                    , Html.p [] [ Html.text (Debug.toString model) ]
                    ]
        }
        |> App.add counter
        |> App.add floatField
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
