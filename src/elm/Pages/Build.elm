{--
Copyright (c) 2020 Target Brands, Inc. All rights reserved.
Use of this source code is governed by the LICENSE file in this repository.
--}


module Pages.Build exposing
    ( clickLogLine
    , clickStep
    , statusToClass
    , statusToString
    , viewBuild
    , viewBuildHistory
    , viewPreview
    )

import Browser.Navigation as Navigation
import DateFormat.Relative exposing (relativeTime)
import Html
    exposing
        ( Html
        , a
        , details
        , div
        , span
        , summary
        , text
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
import Http exposing (Error(..))
import List.Extra exposing (updateIf)
import Logs exposing (SetLogFocus, logFocusFragment, stepToFocusId)
import Pages exposing (Page(..))
import RemoteData exposing (WebData)
import Routes exposing (Route(..))
import SvgBuilder exposing (buildStatusToIcon, recentBuildStatusToIcon, stepStatusToIcon)
import Time exposing (Posix, Zone, millisToPosix)
import Util
import Vela
    exposing
        ( Build
        , BuildNumber
        , Builds
        , FocusFragment
        , Logs
        , Org
        , Repo
        , Status
        , Step
        , StepNumber
        , Steps
        )



-- TYPES


{-| ExpandStep : update action for expanding a build step
-}
type alias ExpandStep msg =
    Org -> Repo -> BuildNumber -> StepNumber -> String -> msg


{-| GetLogsFromBuild : type alias for passing in logs fetch function from Main.elm
-}
type alias GetLogsFromBuild a msg =
    a -> Org -> Repo -> BuildNumber -> StepNumber -> FocusFragment -> Cmd msg


{-| FocusLogs : type alias for passing in url fragment to focus ranges of logs
-}
type alias FocusLogs msg =
    String -> msg


{-| PartialModel : type alias for passing in the main model with the navigation key for pushing log fragment urls
-}
type alias PartialModel a =
    { a | navigationKey : Navigation.Key }



-- VIEW


{-| viewBuild : renders entire build based on current application time
-}
viewBuild : Posix -> Org -> Repo -> WebData Build -> WebData Steps -> Logs -> ExpandStep msg -> FocusLogs msg -> Bool -> Html msg
viewBuild now org repo build steps logs expandAction logFocusAction shiftDown =
    let
        ( buildPreview, buildNumber ) =
            case build of
                RemoteData.Success bld ->
                    ( viewPreview now org repo bld, Just <| String.fromInt bld.number )

                _ ->
                    ( Util.largeLoader, Nothing )

        buildSteps =
            case steps of
                RemoteData.Success steps_ ->
                    viewSteps now org repo buildNumber steps_ logs expandAction logFocusAction shiftDown

                RemoteData.Failure _ ->
                    div [] [ text "Error loading steps... Please try again" ]

                _ ->
                    -- Don't show two loaders
                    if Util.isLoading build then
                        text ""

                    else
                        Util.smallLoader

        markdown =
            [ buildPreview, buildSteps ]
    in
    div [ Util.testAttribute "full-build" ] markdown


{-| viewPreview : renders single build item preview based on current application time
-}
viewPreview : Posix -> Org -> Repo -> Build -> Html msg
viewPreview now org repo build =
    let
        status =
            [ buildStatusToIcon build.status ]

        commit =
            [ text "commit ", a [ href build.source ] [ text <| trimCommitHash build.commit ] ]

        branch =
            [ a [ href <| buildBranchUrl build.clone build.branch ] [ text build.branch ] ]

        sender =
            [ text build.sender ]

        id =
            [ a
                [ Util.testAttribute "build-number"
                , Routes.href <| Routes.Build org repo (String.fromInt build.number) Nothing
                ]
                [ text <| "#" ++ String.fromInt build.number ]
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
                [ div [ class "row" ]
                    [ div [ class "id" ] id
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
                , viewError build
                ]
            ]
    in
    div [ class "build-container", Util.testAttribute "build" ]
        [ div [ class "build", statusClass ] <|
            buildStatusStyles markdown build.status build.number
        ]


{-| viewSteps : sorts and renders build steps
-}
viewSteps : Posix -> Org -> Repo -> Maybe BuildNumber -> Steps -> Logs -> ExpandStep msg -> SetLogFocus msg -> Bool -> Html msg
viewSteps now org repo buildNumber steps logs expandAction logFocusAction shiftDown =
    div [ class "steps" ]
        [ div [ class "-items", Util.testAttribute "steps" ] <|
            List.map
                (\step ->
                    viewStep now org repo buildNumber step steps logs expandAction logFocusAction shiftDown
                )
            <|
                steps
        ]


{-| viewStep : renders single build step
-}
viewStep : Posix -> Org -> Repo -> Maybe BuildNumber -> Step -> Steps -> Logs -> ExpandStep msg -> SetLogFocus msg -> Bool -> Html msg
viewStep now org repo buildNumber step steps logs expandAction logFocusAction shiftDown =
    div [ stepClasses step steps, Util.testAttribute "step" ]
        [ div [ class "-status" ]
            [ div [ class "-icon-container" ] [ viewStepIcon step ] ]
        , div [ classList [ ( "-view", True ), ( "-running", step.status == Vela.Running ) ] ]
            [ viewStepDetails now org repo buildNumber step logs expandAction logFocusAction shiftDown ]
        ]


{-| viewStepDetails : renders build steps detailed information
-}
viewStepDetails : Posix -> Org -> Repo -> Maybe BuildNumber -> Step -> Logs -> ExpandStep msg -> SetLogFocus msg -> Bool -> Html msg
viewStepDetails now org repo buildNumber step logs expandAction logFocusAction shiftDown =
    let
        buildNum =
            Maybe.withDefault "" buildNumber

        stepNumber =
            String.fromInt step.number

        stepSummary =
            [ summary
                [ class "summary"
                , Util.testAttribute "step-header"
                , onClick <| expandAction org repo buildNum stepNumber ("#step:" ++ stepNumber)
                , id <| stepToFocusId <| String.fromInt step.number
                ]
                [ div
                    [ class "-info" ]
                    [ div [ class "-name" ] [ text step.name ]
                    , div [ class "-duration" ] [ text <| Util.formatRunTime now step.started step.finished ]
                    ]
                ]
            , div [ class "logs-container" ] [ Logs.view step logs logFocusAction shiftDown ]
            ]
    in
    details [ class "details", Util.open step.viewing ] stepSummary


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
            div [ class "row" ]
                [ div [ class "error", Util.testAttribute "build-error" ]
                    [ span [ class "label" ] [ text "error:" ]
                    , span [ class "message" ]
                        [ text <|
                            if String.isEmpty build.error then
                                "no error msg"

                            else
                                build.error
                        ]
                    ]
                ]

        _ ->
            text ""


{-| viewBuildHistory : takes the 10 most recent builds and renders icons/links back to them as a widget at the top of the Build page
-}
viewBuildHistory : Posix -> Zone -> Page -> Org -> Repo -> WebData Builds -> Int -> Html msg
viewBuildHistory now timezone page org repo builds limit =
    let
        show =
            case page of
                Pages.Build _ _ _ _ ->
                    True

                _ ->
                    False
    in
    if show then
        case builds of
            RemoteData.Success blds ->
                if List.length blds > 0 then
                    div [ class "build-history", Util.testAttribute "build-history" ] <|
                        List.indexedMap (\idx -> \build -> viewRecentBuild now timezone org repo build idx) <|
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
-}
viewRecentBuild : Posix -> Zone -> Org -> Repo -> Build -> Int -> Html msg
viewRecentBuild now timezone org repo build idx =
    let
        icon =
            recentBuildStatusToIcon build.status idx
    in
    a
        [ class "-build"
        , Routes.href <| Routes.Build org repo (String.fromInt build.number) Nothing
        , attribute "aria-label" <| "go to previous build number " ++ String.fromInt build.number
        ]
        [ icon
        , div [ class "-tooltip", Util.testAttribute "build-history-tooltip" ]
            [ div [ class "-info" ]
                [ div [ class "-line", class "-header" ]
                    [ span [ class "-number" ] [ text <| String.fromInt build.number ]
                    , span [ class "-event" ] [ text build.event ]
                    ]
                , div [ class "-line" ] [ span [ class "-label" ] [ text "started:" ], span [ class "-content" ] [ text <| Util.dateToHumanReadable timezone build.started ] ]
                , div [ class "-line" ] [ span [ class "-label" ] [ text "finished:" ], span [ class "-content" ] [ text <| Util.dateToHumanReadable timezone build.finished ] ]
                , div [ class "-line" ] [ span [ class "-label" ] [ text "duration:" ], span [ class "-content" ] [ text <| Util.formatRunTime now build.started build.finished ] ]
                ]
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

        Vela.Error ->
            "server error"

        Vela.Failure ->
            "failed"


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

        Vela.Error ->
            class "-error"


{-| stepClasses : returns css classes for a particular step
-}
stepClasses : Step -> Steps -> Html.Attribute msg
stepClasses step steps =
    let
        last =
            case List.head <| List.reverse steps of
                Just s ->
                    s.number

                Nothing ->
                    -1
    in
    classList [ ( "step", True ), ( "-line", True ), ( "-last", last == step.number ) ]


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



-- UPDATE HELPERS


{-| clickStep : takes model org repo and step number and fetches step information from the api
-}
clickStep : PartialModel a -> WebData Steps -> Org -> Repo -> BuildNumber -> StepNumber -> GetLogsFromBuild (PartialModel a) msg -> ( WebData Steps, Cmd msg )
clickStep model steps org repo buildNumber stepNumber getLogs =
    let
        stepOpened =
            not <| isViewing steps stepNumber

        focused =
            logFocusExists steps

        ( stepsOut, action ) =
            case steps of
                RemoteData.Success steps_ ->
                    ( RemoteData.succeed <| toggleStepView steps_ stepNumber
                    , getLogs model org repo buildNumber stepNumber Nothing
                    )

                _ ->
                    ( steps, Cmd.none )
    in
    ( stepsOut
    , Cmd.batch <|
        [ action
        , if stepOpened && not focused then
            Navigation.pushUrl model.navigationKey <| logFocusFragment stepNumber []

          else
            Cmd.none
        ]
    )


{-| clickLogLine : takes model and line number and sets the focus on the log line
-}
clickLogLine : WebData Steps -> Navigation.Key -> StepNumber -> Maybe Int -> ( WebData Steps, Cmd msg )
clickLogLine steps navKey stepNumber lineNumber =
    let
        stepOpened =
            Maybe.withDefault False <|
                List.head <|
                    List.map (\step -> not step.viewing) <|
                        List.filter (\step -> String.fromInt step.number == stepNumber) <|
                            RemoteData.withDefault [] steps
    in
    ( steps
    , if stepOpened then
        Navigation.pushUrl navKey <|
            "#step:"
                ++ stepNumber
                ++ (case lineNumber of
                        Just line ->
                            ":"
                                ++ String.fromInt line

                        Nothing ->
                            ""
                   )

      else
        Cmd.none
    )


{-| toggleStepView : takes steps and step number and toggles that steps viewing state
-}
toggleStepView : Steps -> String -> Steps
toggleStepView steps stepNumber =
    List.Extra.updateIf
        (\step -> String.fromInt step.number == stepNumber)
        (\step -> { step | viewing = not step.viewing })
        steps


{-| logFocusExists : takes steps and returns if a line or range has already been focused
-}
logFocusExists : WebData Steps -> Bool
logFocusExists steps =
    (Maybe.withDefault ( Nothing, Nothing ) <|
        List.head <|
            List.map (\step -> step.logFocus) <|
                List.filter
                    (\step -> step.logFocus /= ( Nothing, Nothing ))
                <|
                    RemoteData.withDefault [] steps
    )
        /= ( Nothing, Nothing )


{-| isViewing : takes steps and step number and returns the step viewing state
-}
isViewing : WebData Steps -> StepNumber -> Bool
isViewing steps stepNumber =
    Maybe.withDefault False <|
        List.head <|
            List.map (\step -> step.viewing) <|
                List.filter (\step -> String.fromInt step.number == stepNumber) <|
                    RemoteData.withDefault [] steps