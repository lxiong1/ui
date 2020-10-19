{--
Copyright (c) 2020 Target Brands, Inc. All rights reserved.
Use of this source code is governed by the LICENSE file in this repository.
--}


module Pages.Build.View exposing
    ( statusToClass
    , statusToString
    , viewBuild
    , viewBuildHistory
    , viewPreview
    )

import Ansi.Log
import Array
import DateFormat.Relative exposing (relativeTime)
import FeatherIcons
import Html
    exposing
        ( Html
        , a
        , button
        , code
        , details
        , div
        , em
        , li
        , small
        , span
        , strong
        , summary
        , table
        , td
        , text
        , tr
        , ul
        )
import Html.Attributes
    exposing
        ( attribute
        , class
        , classList
        , href
        , id
        )
import Html.Events exposing (onClick)
import List.Extra exposing (unique)
import Pages exposing (Page(..))
import Pages.Build.Logs
    exposing
        ( ResourceNumber(..)
        , ResourceType(..)
        , decodeAnsi
        , getDownloadLogsFileName
        , getResourceLog
        , logEmpty
        , logFocusStyles
        , logRangeId
        , resourceAndLineToFocusId
        , resourceBottomTrackerFocusId
        , resourceToFocusId
        , resourceTopTrackerFocusId
        , toString
        )
import Pages.Build.Model exposing (BuildModel, Msg(..), PartialModel)
import RemoteData exposing (WebData)
import Routes exposing (Route(..))
import String
import SvgBuilder exposing (buildStatusToIcon, recentBuildStatusToIcon, stepStatusToIcon)
import Time exposing (Posix, Zone)
import Util
import Vela exposing (Build, BuildNumber, Builds, Log, LogFocus, Logs, Org, Repo, Service, Services, Status, Step, StepNumber, Steps, defaultStep)



-- VIEW


{-| viewBuild : renders entire build based on current application time
-}
viewBuild : PartialModel a -> Org -> Repo -> Html Msg
viewBuild model org repo =
    let
        ( buildPreview, buildNumber ) =
            case model.build of
                RemoteData.Success bld ->
                    ( viewPreview model.time org repo bld, String.fromInt bld.number )

                RemoteData.Loading ->
                    ( Util.largeLoader, "" )

                _ ->
                    ( text "", "" )

        logActions =
            model.steps
                |> RemoteData.unwrap (text "")
                    (\_ ->
                        div
                            [ class "buttons"
                            , class "log-actions"
                            , class "flowline-left"
                            , Util.testAttribute "log-actions"
                            ]
                            [ collapseAllStepsButton
                            , expandAllStepsButton org repo buildNumber
                            ]
                    )

        buildData =
            case ( model.steps, model.services ) of
                ( RemoteData.Success steps_, RemoteData.Success services_ ) ->
                    viewPipeline model <| BuildModel org repo buildNumber services_ steps_

                ( RemoteData.Success steps_, RemoteData.Failure _ ) ->
                    viewPipeline model <| BuildModel org repo buildNumber [] steps_

                ( RemoteData.Failure _, RemoteData.Success services_ ) ->
                    viewPipeline model <| BuildModel org repo buildNumber services_ []

                ( RemoteData.Failure _, RemoteData.Failure _ ) ->
                    div [] [ text "Error loading build data... Please try again" ]

                _ ->
                    -- Don't show two loaders
                    if Util.isLoading model.build then
                        text ""

                    else
                        Util.smallLoader

        markdown =
            [ buildPreview
            , logActions
            , buildData
            ]
    in
    div [ Util.testAttribute "full-build" ] markdown


{-| viewPreview : renders single build item preview based on current application time
-}
viewPreview : Posix -> Org -> Repo -> Build -> Html Msg
viewPreview now org repo build =
    let
        buildNumber =
            String.fromInt build.number

        status =
            [ buildStatusToIcon build.status ]

        commit =
            [ text <| String.replace "_" " " build.event
            , text " ("
            , a [ href build.source ] [ text <| trimCommitHash build.commit ]
            , text <| ")"
            ]

        branch =
            [ a [ href <| buildBranchUrl build.clone build.branch ] [ text build.branch ] ]

        sender =
            [ text build.sender ]

        message =
            [ text <| "- " ++ build.message ]

        id =
            [ a
                [ Util.testAttribute "build-number"
                , Routes.href <| Routes.Build org repo buildNumber Nothing
                ]
                [ text <| "#" ++ buildNumber ]
            ]

        age =
            [ text <| relativeTime now <| Time.millisToPosix <| Util.secondsToMillis build.created ]

        duration =
            [ text <| Util.formatRunTime now build.started build.finished ]

        statusClass =
            statusToClass build.status

        markdown =
            [ div [ class "status", Util.testAttribute "build-status", statusClass ] status
            , div [ class "info" ]
                [ div [ class "row -left" ]
                    [ div [ class "id" ] id
                    , div [ class "commit-msg" ] [ strong [] message ]
                    ]
                , div [ class "row" ]
                    [ div [ class "git-info" ]
                        [ div [ class "commit" ] commit
                        , text "on"
                        , div [ class "branch" ] branch
                        , text "by"
                        , div [ class "sender" ] sender
                        ]
                    , div [ class "time-info" ]
                        [ div [ class "age" ] age
                        , span [ class "delimiter" ] [ text "/" ]
                        , div [ class "duration" ] duration
                        ]
                    ]
                , div [ class "row" ]
                    [ viewError build
                    ]
                ]
            ]
    in
    div [ class "build-container", Util.testAttribute "build" ]
        [ div [ class "build", statusClass ] <|
            buildStatusStyles markdown build.status build.number
        ]


{-| viewPipeline : takes build/steps and renders pipeline
-}
viewPipeline : PartialModel a -> BuildModel -> Html Msg
viewPipeline model buildModel =
    case ( List.length buildModel.steps > 0, List.length buildModel.services > 0 ) of
        ( True, True ) ->
            div [ class "steps" ]
                [ div [ class "-items", Util.testAttribute "steps" ] <|
                    if hasStages buildModel.steps then
                        viewServices model buildModel
                            ++ viewStages model buildModel

                    else
                        viewServices model buildModel
                            ++ viewSteps model buildModel
                ]

        ( True, False ) ->
            div [ class "steps" ]
                [ div [ class "-items", Util.testAttribute "steps" ] <|
                    if hasStages buildModel.steps then
                        viewStages model buildModel

                    else
                        viewSteps model buildModel
                ]

        ( False, True ) ->
            div [ class "steps" ]
                [ div [ class "-items", Util.testAttribute "steps" ] <|
                    viewServices model buildModel
                ]

        _ ->
            div [] [ text "No build data to show" ]


{-| viewServices : takes build/services and renders services
-}
viewServices : PartialModel a -> BuildModel -> List (Html Msg)
viewServices model buildModel =
    List.map (\service -> viewService model buildModel service) <| buildModel.services


viewService : PartialModel a -> BuildModel -> Service -> Html Msg
viewService model buildModel service =
    div [ serviceClasses, Util.testAttribute "service" ]
        [ div [ class "-status" ]
            [ div [ class "-icon-container" ] [ viewServiceIcon service ] ]
        , viewServiceDetails model buildModel service
        ]


viewServiceDetails : PartialModel a -> BuildModel -> Service -> Html Msg
viewServiceDetails model buildModel service =
    let
        serviceNumber =
            String.fromInt service.number

        serviceSummary =
            [ summary
                [ class "summary"
                , Util.testAttribute <| "service-header-" ++ serviceNumber
                , onClick <| ExpandService buildModel.org buildModel.repo buildModel.buildNumber serviceNumber
                , id <| resourceToFocusId <| ServiceNum serviceNumber
                ]
                [ div
                    [ class "-info" ]
                    [ div [ class "-name" ] [ text service.name ]
                    , div [ class "-duration" ] [ text <| Util.formatRunTime model.time service.started service.finished ]
                    ]
                , FeatherIcons.chevronDown |> FeatherIcons.withSize 20 |> FeatherIcons.withClass "details-icon-expand" |> FeatherIcons.toHtml []
                ]
            , div [ class "logs-container" ] [ viewLogs model buildModel <| ServiceResource service ]
            ]
    in
    details
        (classList
            [ ( "details", True )
            , ( "-with-border", True )
            , ( "-running", service.status == Vela.Running )
            ]
            :: Util.open service.viewing
        )
        serviceSummary


{-| viewSteps : takes build/steps and renders steps
-}
viewSteps : PartialModel a -> BuildModel -> List (Html Msg)
viewSteps model buildModel =
    List.map (\step -> viewStep model buildModel step) <| buildModel.steps


{-| viewStep : renders single build step
-}
viewStep : PartialModel a -> BuildModel -> Step -> Html Msg
viewStep model buildModel step =
    div [ stepClasses, Util.testAttribute "step" ]
        [ div [ class "-status" ]
            [ div [ class "-icon-container" ] [ viewStepIcon step ] ]
        , viewStepDetails model buildModel step
        ]


{-| viewStepDetails : renders build steps detailed information
-}
viewStepDetails : PartialModel a -> BuildModel -> Step -> Html Msg
viewStepDetails model buildModel step =
    let
        stepNumber =
            String.fromInt step.number

        stepSummary =
            [ summary
                [ class "summary"
                , Util.testAttribute <| "step-header-" ++ stepNumber
                , onClick <| ExpandStep buildModel.org buildModel.repo buildModel.buildNumber stepNumber
                , id <| resourceToFocusId <| StepNum stepNumber
                ]
                [ div
                    [ class "-info" ]
                    [ div [ class "-name" ] [ text step.name ]
                    , div [ class "-duration" ] [ text <| Util.formatRunTime model.time step.started step.finished ]
                    ]
                , FeatherIcons.chevronDown |> FeatherIcons.withSize 20 |> FeatherIcons.withClass "details-icon-expand" |> FeatherIcons.toHtml []
                ]
            , div [ class "logs-container" ] [ viewLogs model buildModel <| StepResource step ]
            ]
    in
    details
        (classList
            [ ( "details", True )
            , ( "-with-border", True )
            , ( "-running", step.status == Vela.Running )
            ]
            :: Util.open step.viewing
        )
        stepSummary


{-| viewStages : takes model and build model and renders steps grouped by stages
-}
viewStages : PartialModel a -> BuildModel -> List (Html Msg)
viewStages model buildModel =
    buildModel.steps
        |> List.map .stage
        |> unique
        |> List.map
            (\stage ->
                buildModel.steps
                    |> List.filter (\step -> step.stage == stage)
                    |> viewStage model buildModel stage
            )


{-| viewStage : takes model, build model and stage and renders the stage steps
-}
viewStage : PartialModel a -> BuildModel -> String -> Steps -> Html Msg
viewStage model buildModel stage steps =
    div
        [ class "stage", Util.testAttribute <| "stage" ]
        [ viewStageDivider model { buildModel | steps = steps } stage
        , steps
            |> List.map (\step -> viewStep model { buildModel | steps = steps } step)
            |> div [ Util.testAttribute <| "stage-" ++ stage ]
        ]


{-| viewStageDivider : renders divider between stage
-}
viewStageDivider : PartialModel a -> BuildModel -> String -> Html Msg
viewStageDivider model buildModel stage =
    if stage /= "init" && stage /= "clone" then
        div [ class "divider", Util.testAttribute <| "stage-divider-" ++ stage ]
            [ div [] [ text stage ] ]

    else
        text ""


{-| hasStages : takes steps and returns true if the pipeline contain stages
-}
hasStages : Steps -> Bool
hasStages steps =
    steps
        |> List.filter (\s -> s.stage /= "")
        |> List.head
        |> Maybe.withDefault defaultStep
        |> (\step -> step.stage /= "")


{-| viewLogs : takes step and logs and renders step logs or step error
-}
viewLogs : PartialModel a -> BuildModel -> ResourceType -> Html Msg
viewLogs model buildModel resourceType =
    case resourceType of
        StepResource step ->
            case step.status of
                Vela.Error ->
                    stepError step

                Vela.Killed ->
                    stepSkipped step

                _ ->
                    let
                        stepNum =
                            StepNum <| String.fromInt step.number

                        resourceLog =
                            getResourceLog (StepResource step) model.logs
                    in
                    viewLogLines buildModel stepNum step.logFocus resourceLog model.followingStep model.shift

        ServiceResource service ->
            case service.status of
                Vela.Error ->
                    serviceError service

                Vela.Killed ->
                    serviceSkipped service

                _ ->
                    let
                        serviceNum =
                            ServiceNum <| String.fromInt service.number

                        resourceLog =
                            getResourceLog (ServiceResource service) model.logs
                    in
                    viewLogLines buildModel serviceNum service.logFocus resourceLog model.followingStep model.shift


{-| viewLogLines : takes stepnumber linefocus log and clickAction shiftDown and renders logs for a build step
-}
viewLogLines : BuildModel -> ResourceNumber -> LogFocus -> Maybe (WebData Log) -> Int -> Bool -> Html Msg
viewLogLines buildModel resourceNumber logFocus maybeLog following shiftDown =
    let
        decodedLog =
            toString maybeLog

        fileName =
            getDownloadLogsFileName buildModel resourceNumber

        resourceNum =
            case resourceNumber of
                StepNum a ->
                    a

                ServiceNum a ->
                    a
    in
    div
        [ class "logs"
        , Util.testAttribute <| "logs-" ++ resourceNum
        ]
    <|
        case Maybe.withDefault RemoteData.NotAsked maybeLog of
            RemoteData.Success _ ->
                if logEmpty decodedLog then
                    [ emptyLogs ]

                else
                    let
                        ( logs, numLines ) =
                            viewLines resourceNumber logFocus decodedLog shiftDown
                    in
                    [ logsHeader resourceNumber fileName decodedLog
                    , logsSidebar resourceNumber following numLines
                    , logs
                    ]

            RemoteData.Failure err ->
                [ code [ Util.testAttribute "logs-error" ] [ text "error fetching logs" ] ]

            _ ->
                [ loadingLogs ]


{-| viewLines : takes step number, line focus information and click action and renders logs
-}
viewLines : ResourceNumber -> LogFocus -> String -> Bool -> ( Html Msg, Int )
viewLines number logFocus decodedLog shiftDown =
    let
        lines =
            if not <| logEmpty decodedLog then
                decodedLog
                    |> decodeAnsi
                    |> Array.indexedMap
                        (\idx line ->
                            Just <|
                                viewLine
                                    (idx + 1)
                                    (Just line)
                                    number
                                    logFocus
                                    shiftDown
                        )
                    |> Array.toList

            else
                [ Just <|
                    viewLine
                        1
                        Nothing
                        number
                        logFocus
                        shiftDown
                ]

        -- update resource filename when adding stages/services
        logs =
            lines
                |> List.filterMap identity

        resourceNum =
            case number of
                StepNum a ->
                    a

                ServiceNum a ->
                    a

        topTracker =
            tr [ class "line", class "tracker" ]
                [ a
                    [ id <|
                        resourceTopTrackerFocusId number
                    , Util.testAttribute <| "top-log-tracker-" ++ resourceNum
                    , Html.Attributes.tabindex -1
                    ]
                    []
                ]

        bottomTracker =
            tr [ class "line", class "tracker" ]
                [ a
                    [ id <|
                        resourceBottomTrackerFocusId number
                    , Util.testAttribute <| "bottom-log-tracker-" ++ resourceNum
                    , Html.Attributes.tabindex -1
                    ]
                    []
                ]
    in
    ( table [ class "logs-table" ] <|
        topTracker
            :: logs
            ++ [ bottomTracker ]
    , List.length lines
    )


{-| viewLine : takes log line and focus information and renders line number button and log
-}
viewLine : Int -> Maybe Ansi.Log.Line -> ResourceNumber -> LogFocus -> Bool -> Html Msg
viewLine lineNumber line resourceNumber logFocus shiftDown =
    let
        itemNum =
            case resourceNumber of
                StepNum a ->
                    a

                ServiceNum a ->
                    a
    in
    tr
        [ Html.Attributes.id <|
            itemNum
                ++ ":"
                ++ String.fromInt lineNumber
        , class "line"
        ]
        [ case line of
            Just l ->
                div
                    [ class "wrapper"
                    , Util.testAttribute <| String.join "-" [ "log", "line", itemNum, String.fromInt lineNumber ]
                    , class <| logFocusStyles logFocus lineNumber
                    ]
                    [ td []
                        [ lineFocusButton resourceNumber logFocus lineNumber shiftDown ]
                    , td [ class "break-text", class "overflow-auto" ]
                        [ code [ Util.testAttribute <| String.join "-" [ "log", "data", itemNum, String.fromInt lineNumber ] ]
                            [ Ansi.Log.viewLine l
                            ]
                        ]
                    ]

            Nothing ->
                text ""
        ]


{-| lineFocusButton : renders button for focusing log line ranges
-}
lineFocusButton : ResourceNumber -> LogFocus -> Int -> Bool -> Html Msg
lineFocusButton itemNumber logFocus lineNumber shiftDown =
    let
        ( itemType, itemNum ) =
            case itemNumber of
                StepNum a ->
                    ( "step", a )

                ServiceNum a ->
                    ( "service", a )
    in
    button
        [ Util.onClickPreventDefault <|
            FocusLogs <|
                logRangeId itemNumber lineNumber logFocus shiftDown
        , Util.testAttribute <| String.join "-" [ "log", "line", "num", itemNum, String.fromInt lineNumber ]
        , id <| resourceAndLineToFocusId itemNumber lineNumber
        , class "line-number"
        , class "button"
        , class "-link"
        , attribute "aria-label" <| "focus " ++ itemType ++ " " ++ itemNum
        ]
        [ span [] [ text <| String.fromInt lineNumber ] ]


{-| collapseAllStepsButton : renders a button for collapsing all steps
-}
collapseAllStepsButton : Html Msg
collapseAllStepsButton =
    Html.button
        [ class "button"
        , class "-link"
        , onClick CollapseAllSteps
        , Util.testAttribute "collapse-all"
        ]
        [ small [] [ text "collapse all" ] ]


{-| expandAllStepsButton : renders a button for expanding all steps
-}
expandAllStepsButton : Org -> Repo -> BuildNumber -> Html Msg
expandAllStepsButton org repo buildNumber =
    Html.button
        [ class "button"
        , class "-link"
        , onClick <| ExpandAllSteps org repo buildNumber
        , Util.testAttribute "expand-all"
        ]
        [ small [] [ text "expand all" ] ]


{-| logsHeader : takes step number, filename and decoded log and renders logs header
-}
logsHeader : ResourceNumber -> String -> String -> Html Msg
logsHeader resourceNumber fileName decodedLog =
    let
        number =
            case resourceNumber of
                StepNum a ->
                    a

                ServiceNum a ->
                    a
    in
    div [ class "buttons", class "logs-header" ]
        [ div
            [ class "line", class "actions" ]
            [ div
                [ class "wrapper"
                , class "buttons"
                , Util.testAttribute <| "logs-header-actions-" ++ number
                ]
                [ downloadLogsButton resourceNumber fileName decodedLog ]
            ]
        ]


{-| logsSidebar : takes step number/following and renders the logs sidebar
-}
logsSidebar : ResourceNumber -> Int -> Int -> Html Msg
logsSidebar resourceNumber following numLines =
    let
        long =
            numLines > 25

        number =
            case resourceNumber of
                StepNum a ->
                    a

                ServiceNum a ->
                    a
    in
    div [ class "logs-sidebar" ]
        [ div [ class "inner-container" ]
            [ div
                [ class "actions"
                , Util.testAttribute <| "logs-sidebar-actions-" ++ number
                ]
              <|
                (if long then
                    [ jumpToTopButton resourceNumber
                    , jumpToBottomButton resourceNumber
                    ]

                 else
                    []
                )
                    ++ [ logFollowButton resourceNumber following ]
            ]
        ]


{-| jumpToBottomButton : renders action button for jumping to the bottom of a step log
-}
jumpToBottomButton : ResourceNumber -> Html Msg
jumpToBottomButton resourceNumber =
    let
        ( resourceNum, resourceType ) =
            case resourceNumber of
                StepNum a ->
                    ( a, "step" )

                ServiceNum a ->
                    ( a, "service" )
    in
    button
        [ class "button"
        , class "-icon"
        , class "tooltip-left"
        , attribute "data-tooltip" "jump to bottom"
        , Util.testAttribute <| "jump-to-bottom-" ++ resourceNum
        , onClick <| FocusOn <| resourceBottomTrackerFocusId resourceNumber
        , attribute "aria-label" <| "jump to bottom of logs for " ++ resourceType ++ " " ++ resourceNum
        ]
        [ FeatherIcons.arrowDown |> FeatherIcons.toHtml [ attribute "role" "img" ] ]


{-| jumpToTopButton : renders action button for jumping to the top of a step log
-}
jumpToTopButton : ResourceNumber -> Html Msg
jumpToTopButton resourceNumber =
    let
        ( resourceNum, resourceType ) =
            case resourceNumber of
                StepNum a ->
                    ( a, "step" )

                ServiceNum a ->
                    ( a, "service" )
    in
    button
        [ class "button"
        , class "-icon"
        , class "tooltip-left"
        , attribute "data-tooltip" "jump to top"
        , Util.testAttribute <| "jump-to-top-" ++ resourceNum
        , onClick <| FocusOn <| resourceTopTrackerFocusId resourceNumber
        , attribute "aria-label" <| "jump to top of logs for " ++ resourceType ++ " " ++ resourceNum
        ]
        [ FeatherIcons.arrowUp |> FeatherIcons.toHtml [ attribute "role" "img" ] ]


{-| downloadLogsButton : renders action button for downloading a step log
-}
downloadLogsButton : ResourceNumber -> String -> String -> Html Msg
downloadLogsButton resourceNumber fileName logs =
    let
        ( resourceNum, resourceType ) =
            case resourceNumber of
                StepNum a ->
                    ( a, "step" )

                ServiceNum a ->
                    ( a, "service" )
    in
    button
        [ class "button"
        , class "-link"
        , Util.testAttribute <| "download-logs-" ++ resourceNum
        , onClick <| DownloadLogs fileName logs
        , attribute "aria-label" <| "download logs for " ++ resourceType ++ " " ++ resourceNum
        ]
        [ text <| "download logs for this " ++ resourceType ]


{-| stepFollowButton : renders button for following step logs
-}
logFollowButton : ResourceNumber -> Int -> Html Msg
logFollowButton resourceNumber following =
    let
        ( resourceInt, resourceNum, resourceType ) =
            case resourceNumber of
                StepNum a ->
                    ( Maybe.withDefault 0 <| String.toInt a, a, "step" )

                ServiceNum a ->
                    ( Maybe.withDefault 0 <| String.toInt a, a, "service" )

        ( tooltip, icon, toFollow ) =
            if following == 0 then
                ( "start following " ++ resourceType ++ " logs", FeatherIcons.play, resourceInt )

            else if following == resourceInt then
                ( "stop following " ++ resourceType ++ " logs", FeatherIcons.pause, 0 )

            else
                ( "start following " ++ resourceType ++ " logs", FeatherIcons.play, resourceInt )
    in
    button
        [ class "button"
        , class "-icon"
        , class "tooltip-left"
        , attribute "data-tooltip" tooltip
        , Util.testAttribute <| "follow-logs-" ++ resourceNum
        , onClick <| FollowStep toFollow -- TODO: fix this
        , attribute "aria-label" <| tooltip ++ " for " ++ resourceType ++ " " ++ resourceNum
        ]
        [ icon |> FeatherIcons.toHtml [ attribute "role" "img" ] ]


{-| serviceError : checks for build error and renders message
-}
serviceError : Service -> Html msg
serviceError service =
    div [ class "service-error", Util.testAttribute "service-error" ]
        [ span [ class "label" ] [ text "error:" ]
        , span [ class "message" ]
            [ text <|
                if String.isEmpty service.error then
                    "no error msg"

                else
                    service.error
            ]
        ]


{-| serviceSkipped : renders message for a skipped service
-}
serviceSkipped : Service -> Html msg
serviceSkipped _ =
    div [ class "service-skipped", Util.testAttribute "service-skipped" ]
        [ span [ class "message" ] [ text "service was skipped" ] ]


{-| stepError : checks for build error and renders message
-}
stepError : Step -> Html msg
stepError step =
    div [ class "message", class "error", Util.testAttribute "step-error" ]
        [ span [] [ text "error:" ]
        , text <|
            if String.isEmpty step.error then
                "null"

            else
                step.error
        ]


{-| loadingLogs : renders message for loading logs
-}
loadingLogs : Html msg
loadingLogs =
    div [ class "message" ]
        [ Util.smallLoaderWithText "loading..." ]


{-| emptyLogs : renders message for empty logs
-}
emptyLogs : Html msg
emptyLogs =
    div [ class "message" ]
        [ text "the build has not written logs to this step yet" ]


{-| stepKilled : renders message for a killed step

    NOTE: not used, but keeping around for future

-}
stepKilled : Step -> Html msg
stepKilled _ =
    div [ class "message", class "error", Util.testAttribute "step-error" ]
        [ text "step was killed" ]


{-| stepSkipped : renders message for a skipped step
-}
stepSkipped : Step -> Html msg
stepSkipped _ =
    div [ class "message", class "error", Util.testAttribute "step-skipped" ]
        [ text "step was skipped" ]


{-| viewServiceIcon : renders a build service status icon
-}
viewServiceIcon : Service -> Html msg
viewServiceIcon service =
    stepStatusToIcon service.status


{-| viewStepIcon : renders a build step status icon
-}
viewStepIcon : Step -> Html msg
viewStepIcon step =
    stepStatusToIcon step.status


{-| viewError : checks for build error and renders message
-}
viewError : Build -> Html msg
viewError build =
    case build.status of
        Vela.Error ->
            div [ class "error", Util.testAttribute "build-error" ]
                [ span [ class "label" ] [ text "error:" ]
                , span [ class "message" ]
                    [ text <|
                        if String.isEmpty build.error then
                            "no error msg"

                        else
                            build.error
                    ]
                ]

        _ ->
            div [] []


{-| viewBuildHistory : takes the 10 most recent builds and renders icons/links back to them as a widget at the top of the Build page
-}
viewBuildHistory : Posix -> Zone -> Page -> Org -> Repo -> WebData Builds -> Int -> Html msg
viewBuildHistory now timezone page org repo builds limit =
    let
        ( show, buildNumber ) =
            case page of
                Pages.Build _ _ b _ ->
                    ( True, Maybe.withDefault -1 <| String.toInt b )

                _ ->
                    ( False, -1 )
    in
    if show then
        case builds of
            RemoteData.Success blds ->
                if List.length blds > 0 then
                    ul [ class "build-history", class "-no-pad", Util.testAttribute "build-history" ] <|
                        List.indexedMap (viewRecentBuild now timezone org repo buildNumber) <|
                            List.take limit blds

                else
                    text ""

            RemoteData.Loading ->
                div [ class "build-history" ] [ Util.smallLoader ]

            RemoteData.NotAsked ->
                div [ class "build-history" ] [ Util.smallLoader ]

            _ ->
                text ""

    else
        text ""


{-| viewRecentBuild : takes recent build and renders status and link to build as a small icon widget

    focusing or hovering the recent build icon will display a build info tooltip

-}
viewRecentBuild : Posix -> Zone -> Org -> Repo -> Int -> Int -> Build -> Html msg
viewRecentBuild now timezone org repo buildNumber idx build =
    li [ class "recent-build" ]
        [ recentBuildLink org repo buildNumber build idx
        , recentBuildTooltip now timezone build
        ]


{-| recentBuildLink : takes time info and build and renders line for redirecting to recent build

    focusing and hovering this element will display the tooltip

-}
recentBuildLink : Org -> Repo -> Int -> Build -> Int -> Html msg
recentBuildLink org repo buildNumber build idx =
    let
        icon =
            recentBuildStatusToIcon build.status idx

        currentBuildClass =
            if buildNumber == build.number then
                class "-current"

            else if buildNumber > build.number then
                class "-older"

            else
                class ""
    in
    a
        [ class "recent-build-link"
        , Util.testAttribute <| "recent-build-link-" ++ String.fromInt buildNumber
        , currentBuildClass
        , Routes.href <| Routes.Build org repo (String.fromInt build.number) Nothing
        , attribute "aria-label" <| "go to previous build number " ++ String.fromInt build.number
        ]
        [ icon
        ]


{-| recentBuildTooltip : takes time info and build and renders tooltip for viewing recent build info

    tooltip is visible when the recent build link is focused or hovered

-}
recentBuildTooltip : Posix -> Zone -> Build -> Html msg
recentBuildTooltip now timezone build =
    div [ class "recent-build-tooltip", Util.testAttribute "build-history-tooltip" ]
        [ ul [ class "info" ]
            [ li [ class "line" ]
                [ span [ class "number" ] [ text <| String.fromInt build.number ]
                , em [] [ text build.event ]
                ]
            , li [ class "line" ] [ span [] [ text "started:" ], text <| Util.dateToHumanReadable timezone build.started ]
            , li [ class "line" ] [ span [] [ text "finished:" ], text <| Util.dateToHumanReadable timezone build.finished ]
            , li [ class "line" ] [ span [] [ text "duration:" ], text <| Util.formatRunTime now build.started build.finished ]
            ]
        ]



-- HELPERS


{-| statusToString : takes build status and returns string
-}
statusToString : Status -> String
statusToString status =
    case status of
        Vela.Pending ->
            "pending"

        Vela.Running ->
            "running"

        Vela.Success ->
            "success"

        Vela.Failure ->
            "failed"

        Vela.Killed ->
            "killed"

        Vela.Error ->
            "server error"


{-| statusToClass : takes build status and returns css class
-}
statusToClass : Status -> Html.Attribute msg
statusToClass status =
    case status of
        Vela.Pending ->
            class "-pending"

        Vela.Running ->
            class "-running"

        Vela.Success ->
            class "-success"

        Vela.Failure ->
            class "-failure"

        Vela.Killed ->
            class "-failure"

        Vela.Error ->
            class "-error"


{-| serviceClasses : returns css classes for a particular service
-}
serviceClasses : Html.Attribute msg
serviceClasses =
    classList [ ( "service", True ), ( "flowline-left", True ) ]


{-| stepClasses : returns css classes for a particular step
-}
stepClasses : Html.Attribute msg
stepClasses =
    classList [ ( "step", True ), ( "flowline-left", True ) ]


{-| buildStatusStyles : takes build markdown and adds styled flair based on running status
-}
buildStatusStyles : List (Html msg) -> Status -> Int -> List (Html msg)
buildStatusStyles markdown buildStatus buildNumber =
    let
        animation =
            case buildStatus of
                Vela.Running ->
                    List.append (topParticles buildNumber) (bottomParticles buildNumber)

                _ ->
                    [ div [ class "build-animation", class "-not-running", statusToClass buildStatus ] []
                    ]
    in
    markdown ++ animation


{-| topParticles : returns an svg frame to parallax scroll on a running build, set to the top of the build
-}
topParticles : Int -> List (Html msg)
topParticles buildNumber =
    let
        -- Use the build number to dynamically set the dash particles, this way builds wont always have the same particle effects
        dashes =
            topBuildNumberDashes buildNumber

        y =
            "0%"
    in
    [ SvgBuilder.buildStatusAnimation "" y [ "-frame-0", "-top", "-cover" ]
    , SvgBuilder.buildStatusAnimation "none" y [ "-frame-0", "-top", "-start" ]
    , SvgBuilder.buildStatusAnimation dashes y [ "-frame-1", "-top", "-running" ]
    , SvgBuilder.buildStatusAnimation dashes y [ "-frame-2", "-top", "-running" ]
    ]


{-| bottomParticles : returns an svg frame to parallax scroll on a running build, set to the bottom of the build
-}
bottomParticles : Int -> List (Html msg)
bottomParticles buildNumber =
    let
        -- Use the build number to dynamically set the dash particles, this way builds wont always have the same particle effects
        dashes =
            bottomBuildNumberDashes buildNumber

        y =
            "100%"
    in
    [ SvgBuilder.buildStatusAnimation "" y [ "-frame-0", "-bottom", "-cover" ]
    , SvgBuilder.buildStatusAnimation "none" y [ "-frame-0", "-bottom", "-start" ]
    , SvgBuilder.buildStatusAnimation dashes y [ "-frame-1", "-bottom", "-running" ]
    , SvgBuilder.buildStatusAnimation dashes y [ "-frame-2", "-bottom", "-running" ]
    ]


{-| topBuildNumberDashes : returns a different particle effect based on a module of the build number
-}
topBuildNumberDashes : Int -> String
topBuildNumberDashes buildNumber =
    case modBy 3 buildNumber of
        1 ->
            "-animation-dashes-1"

        2 ->
            "-animation-dashes-2"

        _ ->
            "-animation-dashes-3"


{-| bottomBuildNumberDashes : returns a different particle effect based on a module of the build number
-}
bottomBuildNumberDashes : Int -> String
bottomBuildNumberDashes buildNumber =
    case modBy 3 buildNumber of
        1 ->
            "-animation-dashes-3"

        2 ->
            "-animation-dashes-1"

        _ ->
            "-animation-dashes-2"


{-| buildBranchUrl : drops '.git' off the clone url and concatenates tree + branch ref
-}
buildBranchUrl : String -> String -> String
buildBranchUrl clone branch =
    String.dropRight 4 clone ++ "/tree/" ++ branch


{-| trimCommitHash : takes the first 7 characters of the full commit hash
-}
trimCommitHash : String -> String
trimCommitHash commit =
    String.left 7 commit
