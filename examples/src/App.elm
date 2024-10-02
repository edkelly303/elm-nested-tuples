module App exposing (..)

import Html
import NestedTuple as NT


start app =
    { emptyComponentsMsg = NT.empty
    , setters = NT.defineSetters
    , updater = NT.define
    , viewer = NT.define
    , init = NT.define
    , parser = NT.define
    , app = app
    }


type Msg msg
    = NoComponentMsg
    | ComponentMsg msg


add component { emptyComponentsMsg, setters, updater, viewer, init, parser, app } =
    { emptyComponentsMsg = NT.cons NoComponentMsg emptyComponentsMsg
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
            (\insertComposerMsg componentModel ( views, emptyComponentsMsg_ ) ->
                let
                    componentMsgToAppMsg componentMsg =
                        insertComposerMsg (ComponentMsg componentMsg) emptyComponentsMsg_

                    view =
                        componentModel
                            |> component.view
                            |> Html.map componentMsgToAppMsg
                in
                ( view :: views
                , emptyComponentsMsg_
                )
            )
            viewer
    , init =
        NT.appender component.init init
    , parser =
        NT.folder
            (\componentModel ctor_ ->
                ctor_ (component.parse componentModel)
            )
            parser
    , app = app
    }


done builder =
    { init = ( builder.app.init, NT.endAppender builder.init )
    , view =
        \( appModel, componentsModel ) ->
            let
                composerMsgInserters =
                    NT.endSetters builder.setters

                collectComponentViews =
                    NT.endFolder2 builder.viewer
            in
            Html.div []
                [ collectComponentViews ( [], builder.emptyComponentsMsg ) composerMsgInserters componentsModel
                    |> Tuple.first
                    |> List.reverse
                    |> Html.div []
                , Html.text (Debug.toString appModel)
                ]
    , update =
        \msg ( appModel, componentsModel ) ->
            let
                updateAllComponents =
                    NT.endMapper2 builder.updater

                newComponentsModel =
                    updateAllComponents msg componentsModel

                update =
                    let
                        parseAllComponents =
                            NT.endFolder builder.parser
                    in
                    parseAllComponents builder.app.update newComponentsModel

                newAppModel =
                    update appModel
            in
            ( newAppModel, newComponentsModel )
    }
