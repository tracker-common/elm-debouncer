effect module Debouncer where { command = MyCmd } exposing (debounce)

{-| Most debouncers follow TEA. That's okay, but you have to muddy your update
function with state and messages that are only for the debouncer. As an effect
manager it presents a much more minimal and user friendly API.

@docs debounce

-}

import Task exposing (Task)
import Dict exposing (Dict)
import Process


type alias State msg =
    Dict String (Task msg msg)


type MyCmd msg
    = Debounce String Float (Task msg msg)


cmdMap : (a -> b) -> MyCmd msg -> MyCmd msg
cmdMap _ (Debounce key delay task) =
    Debounce key delay task


type Msg
    = Execute String


{-| Debounce takes a key for identification, a duration (in seconds), and two
mapping functions that turn your task into a `msg`. The idea is that when the
key and duration are applied in, it should mirror the Task.perform type
signature.

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
                state' =
                    Dict.insert key task state
            in
                if Dict.get key state == Nothing then
                    Process.spawn (eventuallyExecute router key delay)
                        &> onEffects router rest state'
                else
                    onEffects router rest state'


eventuallyExecute : Platform.Router msg Msg -> String -> Float -> Task x ()
eventuallyExecute router key delay =
    Process.sleep delay
        &> (Platform.sendToSelf router (Execute key))


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
    case selfMsg of
        Execute key ->
            maybeExecuteTask router (Dict.get key state)
                &> Task.succeed (Dict.remove key state)


maybeExecuteTask : Platform.Router msg Msg -> Maybe (Task msg msg) -> Task x ()
maybeExecuteTask router mTask =
    case mTask of
        Just task ->
            task
                `Task.andThen` (\msg -> Platform.sendToApp router msg)
                `Task.onError` (\msg -> Platform.sendToApp router msg)

        Nothing ->
            Task.succeed ()
