module Routes exposing
    ( ConcourseRoute
    , Route(..)
    , buildRoute
    , customToString
    , dashboardRoute
    , jobRoute
    , parsePath
    , pipelineRoute
    , toString
    )

import Concourse
import Concourse.Pagination as Pagination
import Navigation exposing (Location)
import QueryString
import Route exposing (..)


type Route
    = Build String String String String
    | Resource String String String
    | Job String String String
    | OneOffBuild String
    | Pipeline String String
    | Dashboard { isHd : Bool }
    | FlySuccess


type alias ConcourseRoute =
    { logical : Route
    , queries : QueryString.QueryString
    , page : Maybe Pagination.Page
    , hash : String
    }



-- pages


build : Route.Route Route
build =
    Build := static "teams" </> string </> static "pipelines" </> string </> static "jobs" </> string </> static "builds" </> string


oneOffBuild : Route.Route Route
oneOffBuild =
    OneOffBuild := static "builds" </> string


resource : Route.Route Route
resource =
    Resource := static "teams" </> string </> static "pipelines" </> string </> static "resources" </> string


job : Route.Route Route
job =
    Job := static "teams" </> string </> static "pipelines" </> string </> static "jobs" </> string


pipeline : Route.Route Route
pipeline =
    Pipeline := static "teams" </> string </> static "pipelines" </> string


dashboard : Bool -> Route.Route Route
dashboard isHd =
    Dashboard { isHd = isHd }
        := static
            (if isHd then
                "hd"

             else
                ""
            )


flySuccess : Route.Route Route
flySuccess =
    FlySuccess := static "fly_success"



-- route utils


buildRoute : Concourse.Build -> String
buildRoute build =
    case build.job of
        Just j ->
            Build j.teamName j.pipelineName j.jobName build.name |> toString

        Nothing ->
            OneOffBuild (Basics.toString build.id) |> toString


jobRoute : Concourse.Job -> String
jobRoute j =
    Job j.teamName j.pipelineName j.name |> toString


pipelineRoute : { a | name : String, teamName : String } -> String
pipelineRoute p =
    Pipeline p.teamName p.name |> toString


dashboardRoute : Bool -> String
dashboardRoute isHd =
    Dashboard { isHd = isHd } |> toString



-- router


sitemap : Router Route
sitemap =
    router
        [ build
        , resource
        , job
        , oneOffBuild
        , pipeline
        , dashboard False
        , dashboard True
        , flySuccess
        ]


match : String -> Route
match =
    Route.match sitemap
        >> Maybe.withDefault (Dashboard { isHd = False })


toString : Route -> String
toString route =
    case route of
        Build teamName pipelineName jobName buildName ->
            reverse build [ teamName, pipelineName, jobName, buildName ]

        Job teamName pipelineName jobName ->
            reverse job [ teamName, pipelineName, jobName ]

        Resource teamName pipelineName resourceName ->
            reverse resource [ teamName, pipelineName, resourceName ]

        OneOffBuild buildId ->
            reverse oneOffBuild [ buildId ]

        Pipeline teamName pipelineName ->
            reverse pipeline [ teamName, pipelineName ]

        Dashboard { isHd } ->
            reverse (dashboard isHd) []

        FlySuccess ->
            reverse flySuccess []


parsePath : Location -> ConcourseRoute
parsePath location =
    let
        queries =
            QueryString.parse location.search
                |> QueryString.remove "csrf_token"
                |> QueryString.remove "token"

        search =
            QueryString.one QueryString.string "search" queries
                |> Maybe.withDefault ""
                |> String.map
                    (\c ->
                        if c == '+' then
                            ' '

                        else
                            c
                    )

        cleanedQueries =
            case search of
                "" ->
                    queries

                term ->
                    queries
                        |> QueryString.remove "search"
                        |> QueryString.add "search" search
    in
    { logical = match <| location.pathname
    , queries = cleanedQueries
    , page = createPageFromSearch location.search
    , hash = location.hash
    }


customToString : ConcourseRoute -> String
customToString route =
    toString route.logical
        ++ (if route.queries == QueryString.empty then
                ""

            else
                QueryString.render route.queries
           )


createPageFromSearch : String -> Maybe Pagination.Page
createPageFromSearch search =
    let
        q =
            QueryString.parse search

        until =
            QueryString.one QueryString.int "until" q

        since =
            QueryString.one QueryString.int "since" q

        limit =
            Maybe.withDefault 100 <| QueryString.one QueryString.int "limit" q
    in
    case ( since, until ) of
        ( Nothing, Just u ) ->
            Just
                { direction = Pagination.Until u
                , limit = limit
                }

        ( Just s, Nothing ) ->
            Just
                { direction = Pagination.Since s
                , limit = limit
                }

        _ ->
            Nothing
