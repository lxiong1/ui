{--
Copyright (c) 2019 Target Brands, Inc. All rights reserved.
Use of this source code is governed by the LICENSE file in this repository.
--}


module Api.Endpoint exposing (Endpoint(..), toUrl)

import Api.Pagination as Pagination
import Url.Builder as UB exposing (QueryParameter, string)
import Vela exposing (AuthParams, BuildNumber, Org, Repo, StepNumber)


{-| apiBase : is the versioned base of all API paths
-}
apiBase : String
apiBase =
    "api/v1"


{-| Endpoint : represents any one unique API endpoint
-}
type Endpoint
    = Authenticate AuthParams
    | Repositories (Maybe Pagination.Page) (Maybe Pagination.PerPage)
    | Repository Org Repo
    | UserSourceRepositories
    | Builds (Maybe Pagination.Page) (Maybe Pagination.PerPage) Org Repo
    | Build Org Repo BuildNumber
    | Steps (Maybe Pagination.Page) (Maybe Pagination.PerPage) Org Repo BuildNumber
    | Step Org Repo BuildNumber StepNumber
    | StepLogs Org Repo BuildNumber StepNumber


{-| toUrl : turns and Endpoint into a URL string
-}
toUrl : String -> Endpoint -> String
toUrl api endpoint =
    case endpoint of
        Authenticate { code, state } ->
            url api [ "authenticate" ] [ UB.string "code" <| Maybe.withDefault "" code, UB.string "state" <| Maybe.withDefault "" state ]

        Repositories maybePage maybePerPage ->
            url api [ "repos" ] <| Pagination.toQueryParams maybePage maybePerPage

        Repository org repo ->
            url api [ "repos", org, repo ] []

        UserSourceRepositories ->
            url api [ "user", "source", "repos" ] []

        Builds maybePage maybePerPage org repo ->
            url api [ "repos", org, repo, "builds" ] <| Pagination.toQueryParams maybePage maybePerPage

        Build org repo buildNumber ->
            url api [ "repos", org, repo, "builds", buildNumber ] []

        Steps maybePage maybePerPage org repo buildNumber ->
            url api [ "repos", org, repo, "builds", buildNumber, "steps" ] <| Pagination.toQueryParams maybePage maybePerPage

        Step org repo buildNumber stepNumber ->
            url api [ "repos", org, repo, "builds", buildNumber, "steps", stepNumber ] []

        StepLogs org repo buildNumber stepNumber ->
            url api [ "repos", org, repo, "builds", buildNumber, "steps", stepNumber, "logs" ] []


{-| url : creates a URL string with the given path segments and query parameters
-}
url : String -> List String -> List QueryParameter -> String
url api segments params =
    -- "authenticate" doesn't live at the base api path
    if List.head segments == Just "authenticate" then
        UB.crossOrigin api segments params

    else
        UB.crossOrigin api (apiBase :: segments) params