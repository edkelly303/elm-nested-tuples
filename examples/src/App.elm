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
        NT.folder 
            (\setter acc -> 
                let 
                    ( thisComponentModel, thisCmd ) = 
                        component.init 
                            (\msg -> ( Just msg, acc.emptyComponentsMsg ))
                            (\msg -> ( Nothing, setter (Just msg) accemptyComponentsMsg ))
                            acc.flags
                in
                { acc 
                | componentsModel = NT.appender thisComponentModel acc.componentsModel
                , appInit = acc.appInit (\msg -> ( Nothing, setter (Just msg) emptyComponentsMsg_ ))
                , cmdList = thisCmd :: acc.cmdList
                }
            ) 
            builder.init
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
            (\setter maybeThisComponentMsg thisComponentModel ( parentUpdate, cmdList, emptyComponentsMsg_ ) ->
                let
                    newParentUpdate =
                        parentUpdate (\msg -> ( Nothing, setter (Just msg) emptyComponentsMsg_ ))

                    newCmdList =
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
                                cmd :: cmdList

                            Nothing ->
                                cmdList
                in
                ( newParentUpdate, newCmdList, emptyComponentsMsg_ )
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
                  setters   (\msg -> ( Nothing, setter (Just msg) emptyComponentsMsg_ ))
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
    { init = 
        \flags -> 
            let 
                initialise = 
                    NT.endFolder builder.init

                { appInit, cmdList, componentsModel } = 
                    initialize 
                        { emptyComponentsMsg = builder.emptyComponentsMsg
                        , flags = flags
                        , appInit = builder.app.init
                        , cmdList = []
                        , componentsModel = NT.define
                        } 
                        builder.setters
                        
                ( appModel, appCmd ) = 
                    appInit toApp flags
            in
            ( ( appModel, componentsModel () )
            , Cmd.batch (appCmd :: cmdList)
            )
    
    , update =
        \( maybeAppMsg, componentsMsg ) ( appModel, componentsModel ) ->
            let
                updateComponentsModel =
                    NT.endMapper3WithContext builder.updater

                newComponentsModel =
                    updateComponentsModel builder.emptyComponentsMsg setters componentsMsg componentsModel

                gatherComponentCmds =
                    NT.endFolder3 builder.cmder

                ( appUpdate, componentCmds, _ ) =
                    gatherComponentCmds
                        ( builder.app.update, [], builder.emptyComponentsMsg )
                        setters
                        componentsMsg
                        componentsModel

                ( newAppModel, appCmd ) =
                    case maybeAppMsg of
                        Just appMsg ->
                            appUpdate toApp appMsg appModel

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
