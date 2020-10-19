{--
Copyright (c) 2020 Target Brands, Inc. All rights reserved.
Use of this source code is governed by the LICENSE file in this repository.
--}


module Pages.Build.Logs exposing
    ( ResourceNumber(..)
    , ResourceType(..)
    , SetLogFocus
    , decodeAnsi
    , focusFragmentToFocusId
    , focusLogs
    , focusResource
    , getCurrentService
    , getCurrentStep
    , getDownloadLogsFileName
    , getResourceLog
    , logEmpty
    , logFocusExists
    , logFocusFragment
    , logFocusStyles
    , logRangeId
    , resourceAndLineToFocusId
    , resourceBottomTrackerFocusId
    , resourceToFocusId
    , resourceTopTrackerFocusId
    , toString
    , unwrapResourceNumber
    )

import Ansi.Log
import Array
import List.Extra exposing (updateIf)
import Pages exposing (Page)
import Pages.Build.Model exposing (BuildModel, Msg(..))
import RemoteData exposing (WebData)
import Vela
    exposing
        ( BuildNumber
        , FocusFragment
        , Log
        , LogFocus
        , Logs
        , Org
        , Repo
        , Service
        , ServiceNumber
        , Services
        , Step
        , StepNumber
        , Steps
        )



-- TYPES


{-| FocusFragment : update action for focusing a log line
-}
type alias SetLogFocus msg =
    String -> msg


type alias GetLogsFromResource a msg =
    a -> BuildModel -> FocusFragment -> Bool -> Cmd msg


type ResourceType
    = StepResource Step
    | ServiceResource Service


type ResourceNumber
    = StepNum StepNumber
    | ServiceNum ServiceNumber


unwrapResourceNumber : ResourceNumber -> ( Int, String )
unwrapResourceNumber resourceNumber =
    -- TODO: use this whereever we are unwrapping resourceNumber
    case resourceNumber of
        StepNum a ->
            ( Maybe.withDefault 0 <| String.toInt a, "step" )

        ServiceNum a ->
            ( Maybe.withDefault 0 <| String.toInt a, "service" )


getResourceLog : ResourceType -> Logs -> Maybe (WebData Log)
getResourceLog resourceType logs =
    logs
        |> List.filter
            (\log ->
                case ( log, resourceType ) of
                    ( RemoteData.Success log_, StepResource step ) ->
                        log_.step_id == step.id

                    ( RemoteData.Success log_, ServiceResource service ) ->
                        log_.service_id == service.id

                    _ ->
                        False
            )
        |> List.head


{-| logFocusFragment : takes resource number and maybe line numbers and produces URL fragment for focusing log ranges
-}
logFocusFragment : ResourceNumber -> List String -> String
logFocusFragment resourceNumber args =
    let
        fragPrefix =
            case resourceNumber of
                StepNum a ->
                    [ "#step", a ]

                ServiceNum a ->
                    [ "#service", a ]
    in
    String.join ":" <| fragPrefix ++ args


{-| resourceToFocusId : takes resource number and returns the resource focus id for auto focusing on page load
-}
resourceToFocusId : ResourceNumber -> String
resourceToFocusId resourceNumber =
    case resourceNumber of
        StepNum a ->
            "step-" ++ a

        ServiceNum a ->
            "service-" ++ a


{-| resourceAndLineToFocusId : takes resource number and line number and returns the line focus id for auto focusing on page load
-}
resourceAndLineToFocusId : ResourceNumber -> Int -> String
resourceAndLineToFocusId resourceNumber lineNumber =
    case resourceNumber of
        StepNum a ->
            "step-" ++ a ++ "-line-" ++ String.fromInt lineNumber

        ServiceNum a ->
            "service-" ++ a ++ "-line-" ++ String.fromInt lineNumber


{-| focusFragmentToFocusId : takes URL fragment and parses it into appropriate line focus id for auto focusing on page load
-}
focusFragmentToFocusId : FocusFragment -> String
focusFragmentToFocusId focusFragment =
    let
        parsed =
            parseFocusFragment focusFragment

        resourceName =
            Maybe.withDefault "resource-" parsed.target
    in
    case ( parsed.targetNumber, parsed.lineA, parsed.lineB ) of
        ( Just targetNum, Just lineA, Nothing ) ->
            resourceName ++ String.fromInt targetNum ++ "-line-" ++ String.fromInt lineA

        ( Just targetNum, Just lineA, Just lineB ) ->
            resourceName ++ String.fromInt targetNum ++ "-line-" ++ String.fromInt lineA ++ "-" ++ String.fromInt lineB

        ( Just targetNum, Nothing, Nothing ) ->
            resourceName ++ String.fromInt targetNum

        _ ->
            ""


{-| logRangeId : takes resource, line, and focus information and returns the fragment for focusing a range of logs
-}
logRangeId : ResourceNumber -> Int -> LogFocus -> Bool -> String
logRangeId resourceNumber lineNumber logFocus shiftDown =
    logFocusFragment resourceNumber <|
        List.map String.fromInt
            (List.sort <|
                case ( shiftDown, logFocus ) of
                    ( True, ( Just lineA, Just lineB ) ) ->
                        if lineNumber < lineA then
                            [ lineNumber, lineB ]

                        else
                            [ lineA, lineNumber ]

                    ( True, ( Just lineA, _ ) ) ->
                        if lineNumber < lineA then
                            [ lineNumber, lineA ]

                        else
                            [ lineA, lineNumber ]

                    _ ->
                        [ lineNumber ]
            )


{-| focusResource : takes FocusFragment URL fragment and expands the appropriate resource to automatically view
-}
focusResource : FocusFragment -> BuildModel -> BuildModel
focusResource focusFragment buildModel =
    let
        parsed =
            parseFocusFragment focusFragment
    in
    case ( parsed.target, parsed.targetNumber ) of
        ( Just "service", Just n ) ->
            let
                newServices =
                    updateIf (\service -> service.number == n)
                        (\service ->
                            { service | viewing = True, logFocus = ( parsed.lineA, parsed.lineB ) }
                        )
                        buildModel.services
            in
            { buildModel
                | services = newServices
            }

        ( Just "step", Just n ) ->
            let
                newSteps =
                    updateIf (\step -> step.number == n)
                        (\step ->
                            { step | viewing = True, logFocus = ( parsed.lineA, parsed.lineB ) }
                        )
                        buildModel.steps
            in
            { buildModel
                | steps = newSteps
            }

        _ ->
            buildModel


{-| getCurrentStep : takes steps and returns the newest running or pending step
-}
getCurrentStep : Steps -> Int
getCurrentStep steps =
    steps
        |> List.filter (\s -> s.status == Vela.Pending || s.status == Vela.Running)
        |> List.map .number
        |> List.sort
        |> List.head
        |> Maybe.withDefault 0


{-| getCurrentService : takes services and returns the newest running or pending service
-}
getCurrentService : Services -> Int
getCurrentService services =
    services
        |> List.filter (\s -> s.status == Vela.Pending || s.status == Vela.Running)
        |> List.map .number
        |> List.sort
        |> List.head
        |> Maybe.withDefault 0


{-| focusLogs : takes model org, repo, build number and log line fragment and loads the appropriate build with focus set on the appropriate log line.
-}
focusLogs : a -> BuildModel -> FocusFragment -> GetLogsFromResource a msg -> ( Page, BuildModel, Cmd msg )
focusLogs model buildModel focusFragment getLogs =
    let
        ( updatedBuildModel, action ) =
            let
                focusedResources =
                    updateLogFocus buildModel focusFragment
            in
            ( focusedResources
            , Cmd.batch
                [ getLogs model buildModel focusFragment False
                ]
            )
    in
    ( Pages.Build buildModel.org buildModel.repo buildModel.buildNumber focusFragment
    , updatedBuildModel
    , action
    )


{-| updateLogFocus : takes line focus and sets a new log line focus for the appropriate resource
-}
updateLogFocus : BuildModel -> FocusFragment -> BuildModel
updateLogFocus buildModel focusFragment =
    let
        parsed =
            parseFocusFragment focusFragment
    in
    case ( parsed.target, parsed.targetNumber ) of
        ( Just "step", Just n ) ->
            { buildModel
                | steps =
                    buildModel.steps
                        |> List.map
                            (\step ->
                                if step.number == n then
                                    { step | viewing = True, logFocus = ( parsed.lineA, parsed.lineB ) }

                                else
                                    -- TODO: this seems wrong..
                                    case clearLogFocus <| StepResource step of
                                        StepResource step_ ->
                                            step_

                                        _ ->
                                            step
                            )
            }

        ( Just "service", Just n ) ->
            { buildModel
                | services =
                    buildModel.services
                        |> List.map
                            (\service ->
                                if service.number == n then
                                    { service | viewing = True, logFocus = ( parsed.lineA, parsed.lineB ) }

                                else
                                    -- TODO: this seems wrong
                                    case clearLogFocus <| ServiceResource service of
                                        ServiceResource service_ ->
                                            service_

                                        _ ->
                                            service
                            )
            }

        _ ->
            buildModel


{-| parseFocusFragment : takes URL fragment and parses it into appropriate line focus chunks
-}
parseFocusFragment : FocusFragment -> { target : Maybe String, targetNumber : Maybe Int, lineA : Maybe Int, lineB : Maybe Int }
parseFocusFragment focusFragment =
    case String.split ":" (Maybe.withDefault "" focusFragment) of
        targetType :: targetNum :: lineA :: lineB :: _ ->
            { target = Just targetType, targetNumber = String.toInt targetNum, lineA = String.toInt lineA, lineB = String.toInt lineB }

        targetType :: targetNum :: lineA :: _ ->
            { target = Just targetType, targetNumber = String.toInt targetNum, lineA = String.toInt lineA, lineB = Nothing }

        targetType :: targetNum :: _ ->
            { target = Just targetType, targetNumber = String.toInt targetNum, lineA = Nothing, lineB = Nothing }

        _ ->
            { target = Nothing, targetNumber = Nothing, lineA = Nothing, lineB = Nothing }


{-| clearLogFocus : takes resource type and clears all log line focus
-}
clearLogFocus : ResourceType -> ResourceType
clearLogFocus resourceType =
    case resourceType of
        StepResource step ->
            StepResource { step | logFocus = ( Nothing, Nothing ) }

        ServiceResource service ->
            ServiceResource { service | logFocus = ( Nothing, Nothing ) }


{-| logFocusStyles : takes linefocus and linenumber and returns the appropriate style for highlighting a focused line
-}
logFocusStyles : LogFocus -> Int -> String
logFocusStyles logFocus lineNumber =
    case logFocus of
        ( Just lineA, Just lineB ) ->
            let
                ( a, b ) =
                    if lineA < lineB then
                        ( lineA, lineB )

                    else
                        ( lineB, lineA )
            in
            if lineNumber >= a && lineNumber <= b then
                "-focus"

            else
                ""

        ( Just lineA, Nothing ) ->
            if lineA == lineNumber then
                "-focus"

            else
                ""

        _ ->
            ""


{-| logFocusExists : takes steps and services and returns if a line or range has already been focused
-}
logFocusExists : WebData Steps -> WebData Services -> Bool
logFocusExists steps services =
    -- TODO: accommodate services
    (Maybe.withDefault ( Nothing, Nothing ) <|
        List.head <|
            List.map (\step -> step.logFocus) <|
                List.filter
                    (\step -> step.logFocus /= ( Nothing, Nothing ))
                <|
                    RemoteData.withDefault [] steps
    )
        /= ( Nothing, Nothing )


{-| logEmpty : takes log string and returns True if content does not exist
-}
logEmpty : String -> Bool
logEmpty log =
    String.isEmpty <| String.replace " " "" log


{-| toString : returns a string from a Maybe Log
-}
toString : Maybe (WebData Log) -> String
toString log =
    case log of
        Just log_ ->
            case log_ of
                RemoteData.Success l ->
                    l.decodedLogs

                _ ->
                    ""

        Nothing ->
            ""


{-| resourceTopTrackerFocusId : takes resource number and returns the line focus id for auto focusing on log follow
-}
resourceTopTrackerFocusId : ResourceNumber -> String
resourceTopTrackerFocusId resourceNumber =
    let
        prefix =
            case resourceNumber of
                StepNum a ->
                    "step-" ++ a

                ServiceNum a ->
                    "service-" ++ a
    in
    prefix ++ "-line-tracker-top"


{-| resourceBottomTrackerFocusId : takes resourced number and returns the line focus id for auto focusing on log follow
-}
resourceBottomTrackerFocusId : ResourceNumber -> String
resourceBottomTrackerFocusId resourceNumber =
    let
        prefix =
            case resourceNumber of
                StepNum a ->
                    "step-" ++ a

                ServiceNum a ->
                    "service-" ++ a
    in
    prefix ++ "-line-tracker-bottom"


{-| getDownloadLogsFileName : takes step information and produces a filename for downloading logs
-}
getDownloadLogsFileName : BuildModel -> ResourceNumber -> String
getDownloadLogsFileName buildModel resourceNumber =
    let
        resource =
            case resourceNumber of
                StepNum a ->
                    [ "step", a ]

                ServiceNum a ->
                    [ "service", a ]
    in
    String.join "-" <| [ buildModel.org, buildModel.repo, buildModel.buildNumber ] ++ resource



-- ANSI


{-| defaultLogModel : struct to represent default model required by ANSI parser
-}
defaultLogModel : Ansi.Log.Model
defaultLogModel =
    { lineDiscipline = Ansi.Log.Cooked
    , lines = Array.empty
    , position = defaultPosition
    , savedPosition = Nothing
    , style = defaultLogStyle
    , remainder = ""
    }


{-| defaultLogStyle : struct to represent default style required by ANSI model
-}
defaultLogStyle : Ansi.Log.Style
defaultLogStyle =
    { foreground = Nothing
    , background = Nothing
    , bold = False
    , faint = False
    , italic = False
    , underline = False
    , blink = False
    , inverted = False
    , fraktur = False
    , framed = False
    }


{-| defaultPosition : default ANSI cursor position
-}
defaultPosition : Ansi.Log.CursorPosition
defaultPosition =
    { row = 0
    , column = 0
    }


{-| decodeAnsi : takes maybe log parses into ansi decoded log line array
see: <https://package.elm-lang.org/packages/vito/elm-ansi>
-}
decodeAnsi : String -> Array.Array Ansi.Log.Line
decodeAnsi log =
    .lines <| Ansi.Log.update log defaultLogModel
