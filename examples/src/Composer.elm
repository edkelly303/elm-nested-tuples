module Composer exposing (..)

import Html
import NestedTuple as NT


start =
    { emptyMsg = NT.empty
    , setters = NT.defineSetters
    , updater = NT.define
    , viewer = NT.define
    , init = NT.define
    }


type Msg msg
    = NoMsg
    | Msg msg


add component prev =
    { emptyMsg = NT.cons NoMsg prev.emptyMsg
    , setters = NT.setter prev.setters
    , updater =
        NT.mapper2
            (\msg model ->
                case msg of
                    Msg msg_ ->
                        component.update msg_ model

                    NoMsg ->
                        model
            )
            prev.updater
    , viewer =
        NT.folder2
            (\msgMapper model ( views, emptyMsg ) ->
                let
                    view =
                        model
                            |> component.view
                            |> Html.map (\msg -> msgMapper (Msg msg) emptyMsg)
                in
                ( view :: views
                , emptyMsg
                )
            )
            prev.viewer
    , init =
        NT.appender component.init prev.init
    }


end prev =
    { init = NT.endAppender prev.init
    , view =
        \model ->
            let
                msgMappers =
                    NT.endSetters prev.setters

                collectAllViews =
                    NT.endFolder2 prev.viewer
            in
            collectAllViews ( [], prev.emptyMsg ) msgMappers model
                |> Tuple.first
                |> List.reverse
                |> Html.div []
    , update =
        \msg model ->
            let
                updateAllElements =
                    NT.endMapper2 prev.updater
            in
            updateAllElements msg model
    }
