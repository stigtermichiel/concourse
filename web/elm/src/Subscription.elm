port module Subscription exposing
    ( Subscription(..)
    , map
    , runSubscription
    , subscribe
    )

import AnimationFrame
import Build.Msgs
import Concourse.BuildEvents as BuildEvents
import Effects
import EventSource
import Keyboard
import Mouse
import Msgs exposing (Msg)
import Scroll
import SubPage.Msgs
import Time
import Window


port newUrl : (String -> msg) -> Sub msg


port tokenReceived : (Maybe String -> msg) -> Sub msg


type Subscription m
    = OnClockTick Time.Time (Time.Time -> m)
    | OnAnimationFrame m
    | OnMouseMove m
    | OnMouseClick m
    | OnKeyPress (Keyboard.KeyCode -> m)
    | OnKeyDown (Keyboard.KeyCode -> m)
    | OnKeyUp (Keyboard.KeyCode -> m)
    | OnScrollFromWindowBottom (Scroll.FromBottom -> m)
    | OnWindowResize (Window.Size -> m)
    | FromEventSource ( String, List String )
    | OnNewUrl (String -> m)
    | OnTokenReceived (Maybe String -> m)
    | Conditionally Bool (Subscription m)
    | WhenPresent (Maybe (Subscription m))


runSubscription : Effects.LayoutDispatch -> Subscription Msg -> Sub Msg
runSubscription disp s =
    case s of
        OnClockTick t m ->
            Time.every t m

        OnAnimationFrame m ->
            AnimationFrame.times (always m)

        OnMouseMove m ->
            Mouse.moves (always m)

        OnMouseClick m ->
            Mouse.clicks (always m)

        OnKeyPress m ->
            Keyboard.presses m

        OnKeyDown m ->
            Keyboard.downs m

        OnKeyUp m ->
            Keyboard.ups m

        OnScrollFromWindowBottom m ->
            Scroll.fromWindowBottom m

        OnWindowResize m ->
            Window.resizes m

        FromEventSource key ->
            case disp of
                Effects.SubPage navIndex ->
                    EventSource.listen key BuildEvents.parseMsg
                        |> Sub.map Build.Msgs.BuildEventsMsg
                        |> Sub.map SubPage.Msgs.BuildMsg
                        |> Sub.map (Msgs.SubMsg navIndex)

                _ ->
                    Sub.none

        OnNewUrl m ->
            newUrl m

        OnTokenReceived m ->
            tokenReceived m

        Conditionally True m ->
            runSubscription disp m

        Conditionally False m ->
            Sub.none

        WhenPresent (Just s) ->
            runSubscription disp s

        WhenPresent Nothing ->
            Sub.none


subscribe : Int -> Subscription m
subscribe build =
    FromEventSource ( "/api/v1/builds/" ++ toString build ++ "/events", [ "end", "event" ] )


map : (m -> n) -> Subscription m -> Subscription n
map f s =
    case s of
        OnClockTick t m ->
            OnClockTick t (m >> f)

        OnAnimationFrame m ->
            OnAnimationFrame (f m)

        OnMouseMove m ->
            OnMouseMove (f m)

        OnMouseClick m ->
            OnMouseClick (f m)

        OnKeyPress m ->
            OnKeyPress (m >> f)

        OnKeyDown m ->
            OnKeyDown (m >> f)

        OnKeyUp m ->
            OnKeyUp (m >> f)

        OnScrollFromWindowBottom m ->
            OnScrollFromWindowBottom (m >> f)

        OnWindowResize m ->
            OnWindowResize (m >> f)

        FromEventSource key ->
            FromEventSource key

        OnNewUrl m ->
            OnNewUrl (m >> f)

        OnTokenReceived m ->
            OnTokenReceived (m >> f)

        Conditionally b m ->
            Conditionally b (map f m)

        WhenPresent (Just s) ->
            WhenPresent (Just (map f s))

        WhenPresent Nothing ->
            WhenPresent Nothing
