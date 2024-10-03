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
            (\insertComposerMsg componentModel ( viewCtor, emptyComponentsMsg_ ) ->
                let
                    componentMsgToAppMsg componentMsg =
                        insertComposerMsg (ComponentMsg componentMsg) emptyComponentsMsg_

                    msgMapper =
                        \msg -> ( Nothing, componentMsgToAppMsg msg )

                    view =
                        component.view componentModel
                            |> Html.map msgMapper
                in
                ( viewCtor { view = view, send = msgMapper, value = component.parse componentModel }
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

                view =
                    collectComponentViews
                        ( builder.app.view, builder.emptyComponentsMsg )
                        composerMsgInserters
                        componentsModel
                        |> Tuple.first
            in
            view (\msg -> ( Just msg, builder.emptyComponentsMsg )) appModel
    , update =
        \( maybeAppMsg, msg ) ( appModel, componentsModel ) ->
            let
                updateAllComponents =
                    NT.endMapper2 builder.updater

                newComponentsModel =
                    updateAllComponents msg componentsModel

                parseAllComponents =
                    NT.endFolder builder.parser

                update =
                    parseAllComponents builder.app.update newComponentsModel

                newAppModel =
                    update maybeAppMsg appModel
            in
            ( newAppModel, newComponentsModel )
    }
