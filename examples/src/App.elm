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
    , subscriber = NT.define
    , app = app
    }


add component builder =
    { emptyComponentsMsg = NT.cons Nothing builder.emptyComponentsMsg
    , setters = NT.setter builder.setters
    , updater =
        NT.mapper3WithContext
            (\emptyComponentsMsg_ setter maybeThisComponentMsg thisComponentModel ->
                case maybeThisComponentMsg of
                    Just thisComponentMsg ->
                        component.update
                            { toSelf = Cmd.map (\msg -> ( Nothing, setter (Just msg) emptyComponentsMsg_ ))
                            , toParent = Cmd.map (\msg -> ( Just msg, emptyComponentsMsg_ ))
                            }
                            thisComponentMsg
                            thisComponentModel
                            |> Tuple.first

                    Nothing ->
                        thisComponentModel
            )
            builder.updater
    , cmder =
        NT.folder3
            (\setter maybeThisComponentMsg thisComponentModel ( cmdList, emptyComponentsMsg_ ) ->
                case maybeThisComponentMsg of
                    Just thisComponentMsg ->
                        let
                            msgMapper =
                                \msg -> ( Nothing, setter (Just msg) emptyComponentsMsg_ )

                            ( _, cmd ) =
                                component.update
                                    { toSelf = Cmd.map msgMapper
                                    , toParent = Cmd.map (\msg -> ( Just msg, emptyComponentsMsg_ ))
                                    }
                                    thisComponentMsg
                                    thisComponentModel
                        in
                        ( cmd :: cmdList, emptyComponentsMsg_ )

                    Nothing ->
                        ( cmdList, emptyComponentsMsg_ )
            )
            builder.cmder
    , viewer =
        NT.folder2
            (\setter thisComponentModel ( viewCtor, emptyComponentsMsg_ ) ->
                let
                    msgMapper =
                        \msg -> ( Nothing, setter (Just msg) emptyComponentsMsg_ )

                    view =
                        component.view thisComponentModel
                            |> Html.map msgMapper
                in
                ( viewCtor { view = view, send = msgMapper }
                , emptyComponentsMsg_
                )
            )
            builder.viewer
    , init =
        NT.appender component.init builder.init
    , subscriber =
        NT.folder2
            (\setter thisComponentModel ( subList, emptyComponentsMsg_ ) ->
                let
                    msgMapper =
                        \msg -> ( Nothing, setter (Just msg) emptyComponentsMsg_ )
                in
                ( (component.subscriptions thisComponentModel |> Sub.map msgMapper) :: subList, emptyComponentsMsg_ )
            )
            builder.subscriber
    , app = builder.app
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
                gatherComponentViews =
                    NT.endFolder2 builder.viewer

                view =
                    gatherComponentViews
                        ( builder.app.view, builder.emptyComponentsMsg )
                        setters
                        componentsModel
                        |> Tuple.first
            in
            view (\msg -> ( Just msg, builder.emptyComponentsMsg )) appModel
    , update =
        \( maybeAppMsg, componentsMsg ) ( appModel, componentsModel ) ->
            let
                updateComponentsModel =
                    NT.endMapper3WithContext builder.updater

                newComponentsModel =
                    updateComponentsModel builder.emptyComponentsMsg setters componentsMsg componentsModel

                gatherComponentCmds =
                    NT.endFolder3 builder.cmder

                componentCmds =
                    gatherComponentCmds ( [], builder.emptyComponentsMsg ) setters componentsMsg componentsModel
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
        \( appModel, componentsModel ) ->
            let
                componentSubscriptions =
                    NT.endFolder2 builder.subscriber ( [], builder.emptyComponentsMsg ) setters componentsModel
                        |> Tuple.first
            in
            Sub.batch
                (builder.app.subscriptions appModel :: componentSubscriptions)
    }
