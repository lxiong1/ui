{--
Copyright (c) 2020 Target Brands, Inc. All rights reserved.
Use of this source code is governed by the LICENSE file in this repository.
--}


module Pages.Build.Update exposing (expandActiveService, expandActiveStep, mergeServices, mergeSteps, update)

import Browser.Dom as Dom
import Browser.Navigation as Navigation
import File.Download as Download
import List.Extra
import Pages.Build.Logs exposing (LogType(..), focusService, focusStep, logFocusFragment)
import Pages.Build.Model
    exposing
        ( GetLogs
        , Msg(..)
        , PartialModel
        )
import RemoteData exposing (RemoteData(..), WebData)
import Task
import Util exposing (overwriteById)
import Vela exposing (ServiceNumber, Services, StepNumber, Steps)



-- UPDATE


update : PartialModel a -> Msg -> GetLogs a msg -> (Result Dom.Error () -> msg) -> ( PartialModel a, Cmd msg )
update model msg getLogs focusResult =
    case msg of
        ExpandService org repo buildNumber serviceNumber ->
            let
                ( services, fetchServiceLogs ) =
                    clickService model.services serviceNumber

                action =
                    if fetchServiceLogs then
                        getLogs.getServiceLogs model org repo buildNumber serviceNumber Nothing True

                    else
                        Cmd.none

                serviceOpened =
                    isViewingService services serviceNumber

                -- service clicked is service being followed
                onFollowedService =
                    model.followingService == (Maybe.withDefault -1 <| String.toInt serviceNumber)

                follow =
                    if onFollowedService && not serviceOpened then
                        -- stop following a service when collapsed
                        0

                    else
                        model.followingService
            in
            ( { model | services = services, followingService = follow }
            , Cmd.batch <|
                [ action
                , if serviceOpened then
                    -- TODO: we have to make logFocusFragment aware of Step vs Service
                    Navigation.pushUrl model.navigationKey <| logFocusFragment (ServiceLog serviceNumber) []

                  else
                    Cmd.none
                ]
            )

        ExpandStep org repo buildNumber stepNumber ->
            let
                ( steps, fetchStepLogs ) =
                    clickStep model.steps stepNumber

                action =
                    if fetchStepLogs then
                        getLogs.getStepLogs model org repo buildNumber stepNumber Nothing True

                    else
                        Cmd.none

                stepOpened =
                    isViewingStep steps stepNumber

                -- step clicked is step being followed
                onFollowedStep =
                    model.followingStep == (Maybe.withDefault -1 <| String.toInt stepNumber)

                follow =
                    if onFollowedStep && not stepOpened then
                        -- stop following a step when collapsed
                        0

                    else
                        model.followingStep
            in
            ( { model | steps = steps, followingStep = follow }
            , Cmd.batch <|
                [ action
                , if stepOpened then
                    Navigation.pushUrl model.navigationKey <| logFocusFragment (StepLog stepNumber) []

                  else
                    Cmd.none
                ]
            )

        FocusLogs url ->
            ( model
            , Navigation.pushUrl model.navigationKey url
            )

        DownloadLogs filename logs ->
            ( model
            , Download.string filename "text" logs
            )

        FollowService service ->
            ( { model | followingService = service }
            , Cmd.none
            )

        FollowStep step ->
            ( { model | followingStep = step }
            , Cmd.none
            )

        CollapseAllSteps ->
            let
                steps =
                    model.steps
                        |> RemoteData.unwrap model.steps
                            (\steps_ -> steps_ |> setAllStepViews False |> RemoteData.succeed)
            in
            ( { model | steps = steps, followingStep = 0 }
            , Cmd.none
            )

        ExpandAllSteps org repo buildNumber ->
            let
                steps =
                    RemoteData.unwrap model.steps
                        (\steps_ -> steps_ |> setAllStepViews True |> RemoteData.succeed)
                        model.steps

                services =
                    RemoteData.unwrap model.services
                        (\services_ -> services_ |> setAllServiceViews True |> RemoteData.succeed)
                        model.services

                -- refresh logs for expanded steps
                actions =
                    Cmd.batch
                        [ getLogs.getStepsLogs model org repo buildNumber (RemoteData.withDefault [] steps) Nothing True
                        , getLogs.getServicesLogs model org repo buildNumber (RemoteData.withDefault [] services) Nothing True
                        ]
            in
            ( { model | steps = steps }
            , actions
            )

        FocusOn id ->
            ( model, Dom.focus id |> Task.attempt focusResult )


{-| clickService : takes services and service number, toggles service view state, and returns whether or not to fetch logs
-}
clickService : WebData Services -> ServiceNumber -> ( WebData Services, Bool )
clickService services serviceNumber =
    let
        ( servicesOut, action ) =
            RemoteData.unwrap ( services, False )
                (\services_ ->
                    ( toggleServiceView serviceNumber services_ |> RemoteData.succeed
                    , True
                    )
                )
                services
    in
    ( servicesOut
    , action
    )


{-| clickStep : takes steps and step number, toggles step view state, and returns whether or not to fetch logs
-}
clickStep : WebData Steps -> StepNumber -> ( WebData Steps, Bool )
clickStep steps stepNumber =
    let
        ( stepsOut, action ) =
            RemoteData.unwrap ( steps, False )
                (\steps_ ->
                    ( toggleStepView stepNumber steps_ |> RemoteData.succeed
                    , True
                    )
                )
                steps
    in
    ( stepsOut
    , action
    )


{-| mergeServices : takes takes current services and incoming service information and merges them, updating old logs and retaining previous state.
-}
mergeServices : Maybe String -> Bool -> WebData Services -> Services -> Services
mergeServices logFocus refresh currentServices incomingServices =
    let
        updatedServices =
            currentServices
                |> RemoteData.unwrap incomingServices
                    (\services ->
                        incomingServices
                            |> List.map
                                (\incomingService ->
                                    let
                                        ( viewing, focus ) =
                                            getServiceInfo services incomingService.number
                                    in
                                    overwriteById
                                        { incomingService
                                            | viewing = viewing
                                            , logFocus = focus
                                        }
                                        services
                                )
                            |> List.filterMap identity
                    )
    in
    -- when not an automatic refresh, respect the url focus
    if not refresh then
        focusService logFocus updatedServices

    else
        updatedServices


{-| mergeSteps : takes takes current steps and incoming step information and merges them, updating old logs and retaining previous state.
-}
mergeSteps : Maybe String -> Bool -> WebData Steps -> Steps -> Steps
mergeSteps logFocus refresh currentSteps incomingSteps =
    let
        updatedSteps =
            currentSteps
                |> RemoteData.unwrap incomingSteps
                    (\steps ->
                        incomingSteps
                            |> List.map
                                (\incomingStep ->
                                    let
                                        ( viewing, focus ) =
                                            getStepInfo steps incomingStep.number
                                    in
                                    overwriteById
                                        { incomingStep
                                            | viewing = viewing
                                            , logFocus = focus
                                        }
                                        steps
                                )
                            |> List.filterMap identity
                    )
    in
    -- when not an automatic refresh, respect the url focus
    if not refresh then
        focusStep logFocus updatedSteps

    else
        updatedSteps


{-| isViewingService : takes services and service number and returns the service viewing state
-}
isViewingService : WebData Services -> ServiceNumber -> Bool
isViewingService services serviceNumber =
    services
        |> RemoteData.withDefault []
        |> List.filter (\service -> String.fromInt service.number == serviceNumber)
        |> List.map .viewing
        |> List.head
        |> Maybe.withDefault False


{-| isViewingStep : takes steps and step number and returns the step viewing state
-}
isViewingStep : WebData Steps -> StepNumber -> Bool
isViewingStep steps stepNumber =
    steps
        |> RemoteData.withDefault []
        |> List.filter (\step -> String.fromInt step.number == stepNumber)
        |> List.map .viewing
        |> List.head
        |> Maybe.withDefault False


{-| toggleServiceView : takes services and service number and toggles that services viewing state
-}
toggleServiceView : String -> Services -> Services
toggleServiceView serviceNumber =
    List.Extra.updateIf
        (\service -> String.fromInt service.number == serviceNumber)
        (\service -> { service | viewing = not service.viewing })


{-| toggleStepView : takes steps and step number and toggles that steps viewing state
-}
toggleStepView : String -> Steps -> Steps
toggleStepView stepNumber =
    List.Extra.updateIf
        (\step -> String.fromInt step.number == stepNumber)
        (\step -> { step | viewing = not step.viewing })


{-| setAllServiceViews : takes steps and value and sets all steps viewing state
-}
setAllServiceViews : Bool -> Services -> Services
setAllServiceViews value =
    List.map (\service -> { service | viewing = value })


{-| setAllStepViews : takes steps and value and sets all steps viewing state
-}
setAllStepViews : Bool -> Steps -> Steps
setAllStepViews value =
    List.map (\step -> { step | viewing = value })


{-| expandActiveService : takes services and sets service viewing state if the service is active
-}
expandActiveService : ServiceNumber -> Services -> Services
expandActiveService serviceNumber services =
    List.Extra.updateIf
        (\service -> (String.fromInt service.number == serviceNumber) && (service.status /= Vela.Pending))
        (\service -> { service | viewing = True })
        services


{-| expandActiveStep : takes steps and sets step viewing state if the step is active
-}
expandActiveStep : StepNumber -> Steps -> Steps
expandActiveStep stepNumber steps =
    List.Extra.updateIf
        (\step -> (String.fromInt step.number == stepNumber) && (step.status /= Vela.Pending))
        (\step -> { step | viewing = True })
        steps


{-| getServiceInfo : takes services and service number and returns the service update information
-}
getServiceInfo : Services -> Int -> ( Bool, ( Maybe Int, Maybe Int ) )
getServiceInfo services serviceNumber =
    services
        |> List.filter (\service -> service.number == serviceNumber)
        |> List.map (\service -> ( service.viewing, service.logFocus ))
        |> List.head
        |> Maybe.withDefault ( False, ( Nothing, Nothing ) )


{-| getStepInfo : takes steps and step number and returns the step update information
-}
getStepInfo : Steps -> Int -> ( Bool, ( Maybe Int, Maybe Int ) )
getStepInfo steps stepNumber =
    steps
        |> List.filter (\step -> step.number == stepNumber)
        |> List.map (\step -> ( step.viewing, step.logFocus ))
        |> List.head
        |> Maybe.withDefault ( False, ( Nothing, Nothing ) )
