effect module Debouncer where { command = MyCmd } exposing (debounce)

{-| Most debouncers follow TEA. That's okay, but you have to muddy your update
function with state and messages that are only for the debouncer. As an effect
manager, it presents a much more minimal and user friendly API.

@docs debounce

-}

import Task exposing (Task)
import Dict exposing (Dict)
import Process


type alias State msg =
    Dict String ( Task msg msg, Process.Id )


type MyCmd msg
    = Debounce String Float (Task msg msg)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f (Debounce key delay task) =
    let
        mapTask =
            Task.mapError f << Task.map f
    in
        Debounce key delay (mapTask task)


type Msg
    = Execute String


{-| Debounce takes a key for identification (which allows you to debounce
multiple things at once), a duration (in milliseconds), and two mapping
functions that turn your task into a `msg`. The idea is that when the key and
duration are applied in, it should mirror the Task.perform type signature.

    perform : (a -> msg) -> (b -> msg) -> Task a b -> Cmd msg
    perform =
      debounce "query" 500

-}
debounce : String -> Float -> (a -> msg) -> (b -> msg) -> Task a b -> Cmd msg
debounce key delay onFail onSuccess task =
    task
        |> Task.map onSuccess
        |> Task.mapError onFail
        |> (command << Debounce key delay)


init : Task Never (State msg)
init =
    Task.succeed (Dict.empty)


(&>) : Task a b -> Task a c -> Task a c
(&>) t1 t2 =
    Task.andThen t1 (\_ -> t2)


onEffects :
    Platform.Router msg Msg
    -> List (MyCmd msg)
    -> State msg
    -> Task Never (State msg)
onEffects router cmds state =
    case cmds of
        [] ->
            Task.succeed state

        (Debounce key delay task) :: rest ->
            let
                updateState v =
                    Dict.insert key v state
            in
                Process.spawn
                    (maybeKill (Maybe.map snd <| Dict.get key state)
                        &> eventuallyExecute router key delay
                    )
                    `Task.andThen` (onEffects router rest
                                        << updateState
                                        << (,) task
                                   )


maybeKill : Maybe Process.Id -> Task x ()
maybeKill pid =
    Maybe.map Process.kill pid
        |> Maybe.withDefault (Task.succeed ())


eventuallyExecute : Platform.Router msg Msg -> String -> Float -> Task x ()
eventuallyExecute router key delay =
    Process.sleep delay
        &> (Platform.sendToSelf router (Execute key))


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
    case selfMsg of
        Execute key ->
            Process.spawn (maybeExecuteTask router (Maybe.map fst <| Dict.get key state))
                &> Task.succeed (Dict.remove key state)


maybeExecuteTask : Platform.Router msg Msg -> Maybe (Task msg msg) -> Task x ()
maybeExecuteTask router mTask =
    case mTask of
        Just task ->
            task
                `Task.andThen` Platform.sendToApp router
                `Task.onError` Platform.sendToApp router

        Nothing ->
            Task.succeed ()
