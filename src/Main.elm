module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick, onInput)
import NestedTuple as NT


main : Program () ComponentsModel ComponentsMsg
main =
    Browser.sandbox components


type alias ComponentsModel =
    ( Int, ( String, () ) )


type alias ComponentsMsg =
    ( Msg CounterMsg, ( Msg String, () ) )


components =
    start
        |> add counter
        |> add textInput
        |> end


type CounterMsg
    = Inc
    | Dec


counter =
    { init = 0
    , update =
        \msg state ->
            case msg of
                Inc ->
                    state + 1

                Dec ->
                    state - 1
    , view =
        \state ->
            Html.button
                [ onClick Inc ]
                [ Html.text (String.fromInt state) ]
    }


textInput =
    { init = "hello"
    , update = \msg state -> msg
    , view =
        \state ->
            Html.div
                []
                [ Html.input [ onInput identity ] [ Html.text state ]
                , Html.text state
                ]
    }


start =
    { emptyMsg = NT.empty
    , accessors = NT.defineAccessors
    , updater = NT.define
    , viewer = NT.define
    , init = NT.define
    }


type Msg msg
    = NoMsg
    | Msg msg


add args builder =
    { emptyMsg = NT.cons NoMsg builder.emptyMsg
    , accessors = NT.accessors builder.accessors
    , updater =
        NT.map2
            (\msg model ->
                case msg of
                    Msg msg_ ->
                        args.update msg_ model

                    NoMsg ->
                        model
            )
            builder.updater
    , viewer =
        NT.fold2
            (\msgMapper model ( listOfViews, emptyMsg ) ->
                ( (model
                    |> args.view
                    |> Html.map (\msg -> msgMapper (Msg msg) emptyMsg)
                  )
                    :: listOfViews
                , emptyMsg
                )
            )
            builder.viewer
    , init =
        builder.init << NT.cons args.init
    }


end builder =
    { init = builder.init NT.empty
    , view =
        \states ->
            let
                setters =
                    builder.accessors
                        |> NT.endAccessors
                        |> .setters

                viewFolder =
                    NT.endFold2 builder.viewer
            in
            viewFolder ( [], builder.emptyMsg ) setters states
                |> Tuple.first
                |> List.reverse
                |> Html.div []
    , update =
        \msg model ->
            let
                updateMapper =
                    NT.endMap2 builder.updater
            in
            updateMapper msg model
    }
