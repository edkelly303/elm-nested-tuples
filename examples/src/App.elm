module App exposing (..)

import Html
import NestedTuple as NT


start app =
    { app = app
    , emptyComponentsMsg = NT.empty
    , setters = NT.defineSetters
    , init = NT.define
    , updater = NT.define
    , cmder = NT.define
    , viewer = NT.define
    , subscriber = NT.define
    }


add component builder =
    { app = builder.app
    , emptyComponentsMsg = NT.cons Nothing builder.emptyComponentsMsg
    , setters = NT.setter builder.setters
    , init =
        NT.appender component.init builder.init
    , updater =
        NT.mapper3WithContext
            (\emptyComponentsMsg_ setter maybeThisComponentMsg thisComponentModel ->
                case maybeThisComponentMsg of
                    Just thisComponentMsg ->
                        component.update
                            (\msg -> ( Just msg, emptyComponentsMsg_ ))
                            (\msg -> ( Nothing, setter (Just msg) emptyComponentsMsg_ ))
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
                            ( _, cmd ) =
                                component.update
                                    (\msg -> ( Just msg, emptyComponentsMsg_ ))
                                    (\msg -> ( Nothing, setter (Just msg) emptyComponentsMsg_ ))
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
            (\setter thisComponentModel ( parentView, emptyComponentsMsg_ ) ->
                let
                    view =
                        component.view
                            (\msg -> ( Just msg, emptyComponentsMsg_ ))
                            (\msg -> ( Nothing, setter (Just msg) emptyComponentsMsg_ ))
                            thisComponentModel
                in
                ( parentView
                    { view = view
                    , send = \msg -> ( Nothing, setter (Just msg) emptyComponentsMsg_ )
                    }
                , emptyComponentsMsg_
                )
            )
            builder.viewer
    , subscriber =
        NT.folder2
            (\setter thisComponentModel ( parentSubscriptions, subList, emptyComponentsMsg_ ) ->
                let
                    subscriptions =
                        component.subscriptions
                            (\msg -> ( Just msg, emptyComponentsMsg_ ))
                            (\msg -> ( Nothing, setter (Just msg) emptyComponentsMsg_ ))
                            thisComponentModel
                in
                ( parentSubscriptions (\msg -> ( Nothing, setter (Just msg) emptyComponentsMsg_ ))
                , subscriptions :: subList
                , emptyComponentsMsg_
                )
            )
            builder.subscriber
    }


done builder =
    let
        setters =
            NT.endSetters builder.setters

        toApp msg =
            ( Just msg, builder.emptyComponentsMsg )
    in
    { init = \_ -> ( ( builder.app.init, NT.endAppender builder.init ), Cmd.none )
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
            view toApp appModel
    , subscriptions =
        \( appModel, componentsModel ) ->
            let
                gatherComponentSubscriptions =
                    NT.endFolder2 builder.subscriber

                ( appSubscriptions, componentSubscriptions, _ ) =
                    gatherComponentSubscriptions
                        ( builder.app.subscriptions, [], builder.emptyComponentsMsg )
                        setters
                        componentsModel
            in
            Sub.batch
                (appSubscriptions toApp appModel :: componentSubscriptions)
    }
