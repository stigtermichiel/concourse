module NewestTopBar
    exposing
        ( Model
        , Msg(..)
        , fetchUser
        , init
        , query
        , update
        , view
        )

import Array
import Concourse
import Concourse.Team
import Concourse.User
import Dom
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HA
    exposing
        ( class
        , css
        , href
        , id
        , placeholder
        , src
        , type_
        , value
        , style
        )
import Html.Styled.Events exposing (..)
import Http
import Keyboard
import LoginRedirect
import Navigation
import NewTopBar.Styles as Styles
import QueryString
import RemoteData exposing (RemoteData)
import Routes
import ScreenSize exposing (ScreenSize(..))
import SearchBar exposing (SearchBar(..))
import Task
import TopBar exposing (userDisplayName)
import UserState exposing (UserState(..))
import Window


-- import StrictEvents exposing (onLeftClickOrShiftLeftClick)


type alias Model =
    { userState : UserState
    , userMenuVisible : Bool
    , searchBar : SearchBar
    , teams : RemoteData.WebData (List Concourse.Team)
    , route : Routes.ConcourseRoute
    }


query : Model -> String
query model =
    case model.searchBar of
        Expanded r ->
            r.query

        _ ->
            ""


type Msg
    = Noop
    | UserFetched (RemoteData.WebData Concourse.User)
    | TeamsFetched (RemoteData.WebData (List Concourse.Team))
    | LogIn
    | LogOut
    | LoggedOut (Result Http.Error ())
    | FilterMsg String
    | FocusMsg
    | BlurMsg
    | SelectMsg Int
    | KeyDown Keyboard.KeyCode
    | ToggleUserMenu
    | ShowSearchInput
    | ScreenResized Window.Size


querySearchForRoute : Routes.ConcourseRoute -> String
querySearchForRoute route =
    QueryString.one QueryString.string "search" route.queries
        |> Maybe.withDefault ""


init : Routes.ConcourseRoute -> ( Model, Cmd Msg )
init route =
    let
        showSearch =
            route.logical == Routes.Dashboard

        searchBar =
            if showSearch then
                Expanded
                    { query = querySearchForRoute route
                    , selectionMade = False
                    , showAutocomplete = False
                    , selection = 0
                    , screenSize = Desktop
                    }
            else
                Invisible
    in
        ( { userState = UserStateUnknown
          , userMenuVisible = False
          , searchBar = searchBar
          , teams = RemoteData.Loading
          , route = route
          }
        , Cmd.batch
            [ fetchUser
            , fetchTeams
            , Task.perform ScreenResized Window.size
            ]
        )


getScreenSize : Window.Size -> ScreenSize
getScreenSize size =
    if size.width < 812 then
        Mobile
    else
        Desktop


queryStringFromSearch : String -> String
queryStringFromSearch query =
    case query of
        "" ->
            QueryString.render QueryString.empty

        query ->
            QueryString.render <|
                QueryString.add "search" query QueryString.empty


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        FilterMsg query ->
            let
                newModel =
                    case model.searchBar of
                        Expanded r ->
                            { model | searchBar = Expanded { r | query = query } }

                        _ ->
                            model
            in
                ( newModel
                , Cmd.batch
                    [ Task.attempt (always Noop) (Dom.focus "search-input-field")
                    , Navigation.modifyUrl (queryStringFromSearch query)
                    ]
                )

        UserFetched user ->
            case user of
                RemoteData.Success user ->
                    ( { model | userState = UserStateLoggedIn user }, Cmd.none )

                _ ->
                    ( { model | userState = UserStateLoggedOut }
                    , Cmd.none
                    )

        LogIn ->
            ( model
            , LoginRedirect.requestLoginRedirect ""
            )

        LogOut ->
            ( model, logOut )

        LoggedOut (Ok ()) ->
            let
                redirectUrl =
                    case model.searchBar of
                        Invisible ->
                            Routes.dashboardHdRoute

                        _ ->
                            Routes.dashboardRoute
            in
                ( { model
                    | userState = UserStateLoggedOut
                    , userMenuVisible = False
                    , teams = RemoteData.Loading
                  }
                , Navigation.newUrl redirectUrl
                )

        LoggedOut (Err err) ->
            flip always (Debug.log "failed to log out" err) <|
                ( model, Cmd.none )

        ToggleUserMenu ->
            ( { model | userMenuVisible = not model.userMenuVisible }, Cmd.none )

        TeamsFetched response ->
            ( { model | teams = response }, Cmd.none )

        FocusMsg ->
            let
                newModel =
                    case model.searchBar of
                        Expanded r ->
                            { model | searchBar = Expanded { r | showAutocomplete = True } }

                        _ ->
                            model
            in
                ( newModel, Cmd.none )

        BlurMsg ->
            let
                newModel =
                    case model.searchBar of
                        Expanded r ->
                            case r.screenSize of
                                Mobile ->
                                    if String.isEmpty r.query then
                                        { model | searchBar = Collapsed }
                                    else
                                        { model | searchBar = Expanded { r | showAutocomplete = False, selectionMade = False, selection = 0 } }

                                Desktop ->
                                    { model | searchBar = Expanded { r | showAutocomplete = False, selectionMade = False, selection = 0 } }

                        _ ->
                            model
            in
                ( newModel, Cmd.none )

        SelectMsg index ->
            let
                newModel =
                    case model.searchBar of
                        Expanded r ->
                            { model | searchBar = Expanded { r | selectionMade = True, selection = index + 1 } }

                        _ ->
                            model
            in
                ( newModel, Cmd.none )

        KeyDown keycode ->
            case model.searchBar of
                Expanded r ->
                    if not r.showAutocomplete then
                        ( { model | searchBar = Expanded { r | selectionMade = False, selection = 0 } }, Cmd.none )
                    else
                        case keycode of
                            -- enter key
                            13 ->
                                if not r.selectionMade then
                                    ( model, Cmd.none )
                                else
                                    let
                                        options =
                                            Array.fromList (autocompleteOptions { query = r.query, teams = model.teams })

                                        index =
                                            (r.selection - 1) % Array.length options

                                        selectedItem =
                                            case Array.get index options of
                                                Nothing ->
                                                    r.query

                                                Just item ->
                                                    item
                                    in
                                        ( { model | searchBar = Expanded { r | selectionMade = False, selection = 0, query = selectedItem } }
                                        , Cmd.none
                                        )

                            -- up arrow
                            38 ->
                                ( { model | searchBar = Expanded { r | selectionMade = True, selection = r.selection - 1 } }, Cmd.none )

                            -- down arrow
                            40 ->
                                ( { model | searchBar = Expanded { r | selectionMade = True, selection = r.selection + 1 } }, Cmd.none )

                            -- escape key
                            27 ->
                                ( model, Task.attempt (always Noop) (Dom.blur "search-input-field") )

                            _ ->
                                ( { model | searchBar = Expanded { r | selectionMade = False, selection = 0 } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ShowSearchInput ->
            showSearchInput model

        ScreenResized size ->
            let
                newSize =
                    getScreenSize size

                newModel =
                    case ( model.searchBar, newSize ) of
                        ( Expanded r, _ ) ->
                            case ( r.screenSize, newSize ) of
                                ( Desktop, Mobile ) ->
                                    if String.isEmpty r.query then
                                        { model | searchBar = Collapsed }
                                    else
                                        { model | searchBar = Expanded { r | screenSize = newSize } }

                                _ ->
                                    { model | searchBar = Expanded { r | screenSize = newSize } }

                        ( Collapsed, Desktop ) ->
                            { model
                                | searchBar =
                                    Expanded
                                        { query = ""
                                        , selectionMade = False
                                        , showAutocomplete = False
                                        , selection = 0
                                        , screenSize = Desktop
                                        }
                            }

                        _ ->
                            model
            in
                ( newModel, Cmd.none )


showSearchInput : Model -> ( Model, Cmd Msg )
showSearchInput model =
    let
        newModel =
            { model
                | searchBar =
                    Expanded
                        { query = ""
                        , selectionMade = False
                        , showAutocomplete = False
                        , selection = 0
                        , screenSize = Mobile
                        }
            }
    in
        case model.searchBar of
            Collapsed ->
                ( newModel, Task.attempt (always Noop) (Dom.focus "search-input-field") )

            _ ->
                ( model, Cmd.none )


viewUserState : { a | userState : UserState, userMenuVisible : Bool } -> List (Html Msg)
viewUserState { userState, userMenuVisible } =
    case userState of
        UserStateUnknown ->
            []

        UserStateLoggedOut ->
            [ Html.div
                [ href "/sky/login"
                , HA.attribute "aria-label" "Log In"
                , id "login-button"
                , onClick LogIn
                , css Styles.menuButton
                ]
                [ Html.div [] [ Html.text "login" ] ]
            ]

        UserStateLoggedIn user ->
            [ Html.div
                [ id "user-id"
                , onClick ToggleUserMenu
                , css Styles.menuButton
                ]
                [ Html.div [ css Styles.userName ] [ Html.text (userDisplayName user) ] ]
            ]
                ++ (if userMenuVisible then
                        [ Html.div
                            [ HA.attribute "aria-label" "Log Out"
                            , onClick LogOut
                            , css Styles.logoutButton
                            , id "logout-button"
                            ]
                            [ Html.div [] [ Html.text "logout" ] ]
                        ]
                    else
                        []
                   )


searchInput : { a | query : String, screenSize : ScreenSize } -> List (Html Msg)
searchInput { query, screenSize } =
    [ Html.div [ css Styles.searchForm ] <|
        [ Html.input
            [ id "search-input-field"
            , type_ "text"
            , placeholder "search"
            , onInput FilterMsg
            , onFocus FocusMsg
            , onBlur BlurMsg
            , value query
            , css <| Styles.searchInput screenSize
            ]
            []
        , Html.span
            [ css <| Styles.searchClearButton (not <| String.isEmpty query)
            , id "search-clear-button"
            , onClick (FilterMsg "")
            ]
            []
        ]
    ]


view : Model -> Html Msg
view model =
    Html.div [ id "top-bar-app", style Styles.topBarCSS ] <|
        viewConcourseLogo
            ++ (List.intersperse viewBreadcrumbSeparator
                    (case model.route.logical of
                        Routes.Pipeline teamName pipelineName ->
                            viewPipelineBreadcrumb (Routes.toString model.route.logical) pipelineName

                        Routes.Build teamName pipelineName jobName buildNumber ->
                            viewPipelineBreadcrumb (Routes.toString (Routes.Pipeline teamName pipelineName)) pipelineName
                                ++ viewJobBreadcrumb (Routes.toString (Routes.Job teamName pipelineName jobName))

                        Routes.Resource teamName pipelineName resourceName ->
                            viewPipelineBreadcrumb (Routes.toString (Routes.Pipeline teamName pipelineName)) pipelineName
                                ++ (viewResourceBreadcrumb resourceName)

                        Routes.Job teamName pipelineName jobName ->
                            viewPipelineBreadcrumb (Routes.toString (Routes.Pipeline teamName pipelineName)) pipelineName

                        _ ->
                            []
                    )
               )


viewConcourseLogo : List (Html Msg)
viewConcourseLogo =
    [ Html.a
        [ style Styles.concourseLogoCSS, href "#" ]
        []
    ]


breadcrumbComponent : String -> String -> List (Html Msg)
breadcrumbComponent componentType name =
    [ Html.div
        [ style (Styles.breadcrumbComponentCSS componentType) ]
        []
    , Html.text <| decodeName name
    ]


viewBreadcrumbSeparator : Html Msg
viewBreadcrumbSeparator =
    Html.li [ class "breadcrumb-separator", style Styles.breadcrumbContainerCSS ] [ Html.text "/" ]


viewPipelineBreadcrumb : String -> String -> List (Html Msg)
viewPipelineBreadcrumb url pipelineName =
    [ Html.li [ style Styles.breadcrumbContainerCSS, id "breadcrumb-pipeline" ]
        [ Html.a
            [ href url ]
          <|
            breadcrumbComponent "pipeline" pipelineName
        ]
    ]


viewJobBreadcrumb : String -> List (Html Msg)
viewJobBreadcrumb jobName =
    [ Html.li [ id "breadcrumb-job", style Styles.breadcrumbContainerCSS ] <| breadcrumbComponent "job" jobName ]


viewResourceBreadcrumb : String -> List (Html Msg)
viewResourceBreadcrumb resourceName =
    [ Html.li [ id "breadcrumb-resource", style Styles.breadcrumbContainerCSS ] <| breadcrumbComponent "resource" resourceName ]


decodeName : String -> String
decodeName name =
    Maybe.withDefault name (Http.decodeUri name)


viewMiddleSection : Model -> List (Html Msg)
viewMiddleSection model =
    case model.searchBar of
        Invisible ->
            []

        Collapsed ->
            [ Html.div [ css <| Styles.middleSection model.searchBar ]
                [ Html.a
                    [ id "search-button"
                    , onClick ShowSearchInput
                    , css Styles.searchButton
                    ]
                    []
                ]
            ]

        Expanded r ->
            [ Html.div [ css <| Styles.middleSection model.searchBar ] <|
                (searchInput r
                    ++ (if r.showAutocomplete then
                            [ Html.ul
                                [ css <| Styles.searchOptionsList r.screenSize ]
                                (viewAutocomplete
                                    { query = r.query
                                    , teams = model.teams
                                    , selectionMade = r.selectionMade
                                    , selection = r.selection
                                    , screenSize = r.screenSize
                                    }
                                )
                            ]
                        else
                            []
                       )
                )
            ]


viewBreadcrumbs : Model -> List (Html Msg)
viewBreadcrumbs model =
    [ Html.div [] [] ]


viewAutocomplete :
    { a
        | query : String
        , teams : RemoteData.WebData (List Concourse.Team)
        , selectionMade : Bool
        , selection : Int
        , screenSize : ScreenSize
    }
    -> List (Html Msg)
viewAutocomplete r =
    let
        options =
            autocompleteOptions r
    in
        options
            |> List.indexedMap
                (\index option ->
                    let
                        active =
                            r.selectionMade && index == (r.selection - 1) % List.length options
                    in
                        Html.li
                            [ onMouseDown (FilterMsg option)
                            , onMouseOver (SelectMsg index)
                            , css <| Styles.searchOption { screenSize = r.screenSize, active = active }
                            ]
                            [ Html.text option ]
                )


viewUserInfo : Model -> List (Html Msg)
viewUserInfo model =
    case model.searchBar of
        Expanded r ->
            case r.screenSize of
                Mobile ->
                    []

                Desktop ->
                    [ Html.div [ css Styles.userInfo ] (viewUserState model) ]

        _ ->
            [ Html.div [ css Styles.userInfo ] (viewUserState model) ]


fetchUser : Cmd Msg
fetchUser =
    Cmd.map UserFetched <|
        RemoteData.asCmd Concourse.User.fetchUser


fetchTeams : Cmd Msg
fetchTeams =
    Cmd.map TeamsFetched <|
        RemoteData.asCmd Concourse.Team.fetchTeams


autocompleteOptions : { a | query : String, teams : RemoteData.WebData (List Concourse.Team) } -> List String
autocompleteOptions { query, teams } =
    case String.trim query of
        "" ->
            [ "status: ", "team: " ]

        "status:" ->
            [ "status: paused", "status: pending", "status: failed", "status: errored", "status: aborted", "status: running", "status: succeeded" ]

        "team:" ->
            case teams of
                RemoteData.Success ts ->
                    List.map (\team -> "team: " ++ team.name) <| List.take 10 ts

                _ ->
                    []

        _ ->
            []


logOut : Cmd Msg
logOut =
    Task.attempt LoggedOut Concourse.User.logOut
