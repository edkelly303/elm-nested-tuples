module App exposing (..)

import Html
import NestedTuple as NT


start app =
    { emptyComponentsMsg = NT.empty
    , setters = NT.defineSetters
    , updater = NT.define
    , cmder = NT.define
    , viewer = NT.define
    , init = NT.define
    , app = app
    }


type Msg msg
    = NoComponentMsg
    | ComponentMsg msg


add component { emptyComponentsMsg, setters, updater, cmder, viewer, init, app } =
    { emptyComponentsMsg = NT.cons NoComponentMsg emptyComponentsMsg
    , setters = NT.setter setters
    , updater =
        NT.mapper3WithContext
            (\emptyComponentsMsg_ setter composerMsg componentModel ->
                case composerMsg of
                    ComponentMsg componentMsg ->
                        component.update
                            { toSelf = Cmd.map (\msg -> ( Nothing, setter (ComponentMsg msg) emptyComponentsMsg_ ))
                            , toParent = Cmd.map (\msg -> ( Just msg, emptyComponentsMsg_ ))
                            }
                            componentMsg
                            componentModel
                            |> Tuple.first

                    NoComponentMsg ->
                        componentModel
            )
            updater
    , cmder =
        NT.folder3
            (\setter composerMsg componentModel ( cmdList, emptyComponentsMsg_ ) ->
                case composerMsg of
                    ComponentMsg componentMsg ->
                        let
                            msgMapper =
                                \msg -> ( Nothing, setter (ComponentMsg msg) emptyComponentsMsg_ )

                            ( _, cmd ) =
                                component.update
                                    { toSelf = Cmd.map msgMapper
                                    , toParent = Cmd.map (\msg -> ( Just msg, emptyComponentsMsg_ ))
                                    }
                                    componentMsg
                                    componentModel
                        in
                        ( cmd :: cmdList, emptyComponentsMsg_ )

                    NoComponentMsg ->
                        ( cmdList, emptyComponentsMsg_ )
            )
            cmder
    , viewer =
        NT.folder2
            (\setter componentModel ( viewCtor, emptyComponentsMsg_ ) ->
                let
                    msgMapper =
                        \msg -> ( Nothing, setter (ComponentMsg msg) emptyComponentsMsg_ )

                    view =
                        component.view componentModel
                            |> Html.map msgMapper
                in
                ( viewCtor { view = view, send = msgMapper }
                , emptyComponentsMsg_
                )
            )
            viewer
    , init =
        NT.appender component.init init
    , app = app
    }


done builder =
    let
        setters =
            NT.endSetters builder.setters
    in
    { init = \_ -> ( ( builder.app.init, NT.endAppender builder.init ), Cmd.none )
    , view =
        \( appModel, componentsModel ) ->
            let
                collectComponentViews =
                    NT.endFolder2 builder.viewer

                view =
                    collectComponentViews
                        ( builder.app.view, builder.emptyComponentsMsg )
                        setters
                        componentsModel
                        |> Tuple.first
            in
            view (\msg -> ( Just msg, builder.emptyComponentsMsg )) appModel
    , update =
        \( maybeAppMsg, componentsMsg ) ( appModel, componentsModel ) ->
            let
                updateAllComponents =
                    NT.endMapper3WithContext builder.updater

                newComponentsModel =
                    updateAllComponents builder.emptyComponentsMsg setters componentsMsg componentsModel

                gatherAllCmds =
                    NT.endFolder3 builder.cmder

                componentCmds =
                    gatherAllCmds ( [], builder.emptyComponentsMsg ) setters componentsMsg componentsModel
                        |> Tuple.first

                ( newAppModel, appCmd ) =
                    case maybeAppMsg of
                        Just appMsg ->
                            builder.app.update appMsg appModel

                        Nothing ->
                            ( appModel, Cmd.none )
            in
            ( ( newAppModel, newComponentsModel ), Cmd.batch (appCmd :: componentCmds) )
    , subscriptions =
        \( appModel, componentsModel ) -> Sub.none
    }
