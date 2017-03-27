effect module Debouncer where { command = MyCmd } exposing (debounce)

{-| Most debouncers follow TEA. That's okay, but you have to muddy your update
function with state and messages that are only for the debouncer. As an effect
manager, it presents a much more minimal and user friendly API.

@docs debounce

-}

{-
   MIT License

   Copyright (c) 2016 Matthew Conger-Eldeen

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}

import Task exposing (Task)
import Dict exposing (Dict)
import Tuple
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
multiple things at once), a duration (in milliseconds), and a mapping
functions that turn the `Result` from running your task into a `msg`. The idea
is that when the key and duration are applied in, it should mirror the
`Task.attempt` type signature.

    debouncedAttempt : (Result x a -> msg) -> Task x a -> Cmd msg
    debouncedAttempt =
      debounce "query" 500

-}
debounce : String -> Float -> (Result x a -> msg) -> Task x a -> Cmd msg
debounce key delay msg task =
    task
        |> Task.andThen (Task.succeed << msg << Ok)
        |> Task.onError (Task.succeed << msg << Err)
        |> (command << Debounce key delay)


init : Task Never (State msg)
init =
    Task.succeed (Dict.empty)


(&>) : Task a b -> Task a c -> Task a c
(&>) t1 t2 =
    Task.andThen (\_ -> t2) t1


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
                maybeKill (Maybe.map Tuple.second <| Dict.get key state)
                    &> Process.spawn (eventuallyExecute router key delay)
                    |> Task.andThen
                        (onEffects router rest
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
            Process.spawn (maybeExecuteTask router (Maybe.map Tuple.first <| Dict.get key state))
                &> Task.succeed (Dict.remove key state)


maybeExecuteTask : Platform.Router msg Msg -> Maybe (Task msg msg) -> Task x ()
maybeExecuteTask router mTask =
    case mTask of
        Just task ->
            task
                |> Task.andThen (Platform.sendToApp router)
                |> Task.onError (Platform.sendToApp router)

        Nothing ->
            Task.succeed ()
