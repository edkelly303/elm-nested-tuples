module Composer exposing (..)

import Html
import NestedTuple as NT


start =
    { emptyAppMsg = NT.empty
    , setters = NT.defineSetters
    , updater = NT.define
    , viewer = NT.define
    , init = NT.define
    }


type Msg msg
    = NoComponentMsg
    | ComponentMsg msg


add component { emptyAppMsg, setters, updater, viewer, init } =
    { emptyAppMsg = NT.cons NoComponentMsg emptyAppMsg
    , setters = NT.setter setters
    , updater =
        NT.mapper2
            (\composerMsg componentModel ->
                case composerMsg of
                    ComponentMsg componentMsg ->
                        component.update componentMsg componentModel

                    NoComponentMsg ->
                        componentModel
            )
            updater
    , viewer =
        NT.folder2
            (\insertComposerMsg componentModel ( views, emptyAppMsg_ ) ->
                let
                    componentMsgToAppMsg componentMsg =
                        insertComposerMsg (ComponentMsg componentMsg) emptyAppMsg_

                    view =
                        componentModel
                            |> component.view
                            |> Html.map componentMsgToAppMsg
                in
                ( view :: views
                , emptyAppMsg_
                )
            )
            viewer
    , init =
        NT.appender component.init init
    }


end components =
    { init = NT.endAppender components.init
    , view =
        \model ->
            let
                composerMsgInserters =
                    NT.endSetters components.setters

                collectAllViews =
                    NT.endFolder2 components.viewer
            in
            collectAllViews ( [], components.emptyAppMsg ) composerMsgInserters model
                |> Tuple.first
                |> List.reverse
                |> Html.div []
    , update =
        \msg model ->
            let
                updateAllComponents =
                    NT.endMapper2 components.updater
            in
            updateAllComponents msg model
    }
