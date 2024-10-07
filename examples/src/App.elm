module App exposing (..)

import NestedTuple as NT


start app =
    { app = app
    , emptyComponentsMsg = NT.empty
    , setters = NT.defineSetters
    , init = NT.define
    , updater = NT.define
    , viewer = NT.define
    , subscriber = NT.define
    }


add component builder =
    { app = builder.app
    , emptyComponentsMsg = NT.cons Nothing builder.emptyComponentsMsg
    , setters = NT.setter builder.setters
    , init =
        NT.folder
            (\setter acc ->
                let
                    ( thisComponentModel, thisCmd ) =
                        component.init
                            (\msg -> ( Just msg, acc.emptyComponentsMsg ))
                            (\msg -> ( Nothing, setter (Just msg) acc.emptyComponentsMsg ))
                            acc.flags
                in
                { componentsModel = NT.appender thisComponentModel acc.componentsModel
                , appInit = acc.appInit (\msg -> ( Nothing, setter (Just msg) acc.emptyComponentsMsg ))
                , cmdList = thisCmd :: acc.cmdList
                , flags = acc.flags
                , emptyComponentsMsg = acc.emptyComponentsMsg
                }
            )
            builder.init
    , updater =
        NT.folder3
            (\setter maybeThisComponentMsg thisComponentModel acc ->
                let
                    sendToComponent =
                        \msg -> ( Nothing, setter (Just msg) acc.emptyComponentsMsg )

                    sendToApp =
                        \msg -> ( Just msg, acc.emptyComponentsMsg )

                    appUpdate =
                        acc.appUpdate sendToComponent

                    ( newThisComponentModel, thisCmd ) =
                        case maybeThisComponentMsg of
                            Just thisComponentMsg ->
                                component.update
                                    sendToApp
                                    sendToComponent
                                    thisComponentMsg
                                    thisComponentModel

                            Nothing ->
                                ( thisComponentModel, Cmd.none )
                in
                { appUpdate = appUpdate
                , componentCmdsList = thisCmd :: acc.componentCmdsList
                , newComponentsModel = NT.appender newThisComponentModel acc.newComponentsModel
                , emptyComponentsMsg = acc.emptyComponentsMsg
                }
            )
            builder.updater
    , viewer =
        NT.folder2
            (\setter thisComponentModel acc ->
                let
                    sendToComponent =
                        \msg -> ( Nothing, setter (Just msg) acc.emptyComponentsMsg )

                    sendToApp =
                        \msg -> ( Just msg, acc.emptyComponentsMsg )

                    componentView =
                        component.view
                            sendToApp
                            sendToComponent
                            thisComponentModel
                in
                { appView = acc.appView componentView sendToComponent
                , emptyComponentsMsg = acc.emptyComponentsMsg
                }
            )
            builder.viewer
    , subscriber =
        NT.folder2
            (\setter thisComponentModel acc ->
                let
                    sendToComponent =
                        \msg -> ( Nothing, setter (Just msg) acc.emptyComponentsMsg )

                    sendToApp =
                        \msg -> ( Just msg, acc.emptyComponentsMsg )

                    subscriptions =
                        component.subscriptions
                            sendToApp
                            sendToComponent
                            thisComponentModel
                in
                { appSubscriptions = acc.appSubscriptions sendToComponent
                , componentSubscriptionsList = subscriptions :: acc.componentSubscriptionsList
                , emptyComponentsMsg = acc.emptyComponentsMsg
                }
            )
            builder.subscriber
    }


done builder =
    let
        setters =
            NT.endSetters builder.setters

        sendToApp msg =
            ( Just msg, builder.emptyComponentsMsg )
    in
    { init =
        \flags ->
            let
                initialise =
                    NT.endFolder builder.init

                { appInit, cmdList, componentsModel } =
                    initialise
                        { emptyComponentsMsg = builder.emptyComponentsMsg
                        , flags = flags
                        , appInit = builder.app.init
                        , cmdList = []
                        , componentsModel = NT.define
                        }
                        setters

                ( appModel, appCmd ) =
                    appInit sendToApp flags
            in
            ( ( appModel, NT.endAppender componentsModel )
            , Cmd.batch (appCmd :: cmdList)
            )
    , update =
        \( maybeAppMsg, componentsMsg ) ( appModel, componentsModel ) ->
            let
                gatherUpdates =
                    NT.endFolder3 builder.updater

                { appUpdate, componentCmdsList, newComponentsModel } =
                    gatherUpdates
                        { appUpdate = builder.app.update
                        , componentCmdsList = []
                        , newComponentsModel = NT.define
                        , emptyComponentsMsg = builder.emptyComponentsMsg
                        }
                        setters
                        componentsMsg
                        componentsModel

                ( newAppModel, appCmd ) =
                    case maybeAppMsg of
                        Just appMsg ->
                            appUpdate sendToApp appMsg appModel

                        Nothing ->
                            ( appModel, Cmd.none )
            in
            ( ( newAppModel, NT.endAppender newComponentsModel )
            , Cmd.batch (appCmd :: componentCmdsList)
            )
    , view =
        \( appModel, componentsModel ) ->
            let
                gatherComponentViews =
                    NT.endFolder2 builder.viewer

                { appView } =
                    gatherComponentViews
                        { appView = builder.app.view
                        , emptyComponentsMsg = builder.emptyComponentsMsg
                        }
                        setters
                        componentsModel
            in
            appView sendToApp appModel
    , subscriptions =
        \( appModel, componentsModel ) ->
            let
                gatherSubscriptions =
                    NT.endFolder2 builder.subscriber

                { appSubscriptions, componentSubscriptionsList } =
                    gatherSubscriptions
                        { appSubscriptions = builder.app.subscriptions
                        , componentSubscriptionsList = []
                        , emptyComponentsMsg = builder.emptyComponentsMsg
                        }
                        setters
                        componentsModel
            in
            Sub.batch
                (appSubscriptions sendToApp appModel :: componentSubscriptionsList)
    }
