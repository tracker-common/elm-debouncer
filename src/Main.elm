module Main exposing (main)

import Html.App as App
import Html exposing (Html)
import Html.Events as Html
import Html.Attributes as Attr
import Task exposing (Task)
import Debouncer


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    ( String, String )


type Msg
    = QueryUpdated String
    | ResultComputed String


init : ( Model, Cmd Msg )
init =
    ( ( "", "" ), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( query, result ) =
    case msg of
        QueryUpdated query' ->
            ( query', result ) ! [ computeResult query' ]

        ResultComputed result' ->
            ( query, result' ) ! []


computeResult : String -> Cmd Msg
computeResult query =
    lazyTask (\_ -> spin 5000000 query)
        |> debounceQuery identity ResultComputed


debounceQuery : (a -> msg) -> (b -> msg) -> Task a b -> Cmd msg
debounceQuery =
    Debouncer.debounce "query" 300


lazyTask : (() -> a) -> Task x a
lazyTask f =
    Task.succeed f
        `Task.andThen` (\g -> Task.succeed <| g ())


spin : Int -> a -> a
spin count value =
    if count == 0 then
        value
    else
        spin (count - 1) value


view : Model -> Html Msg
view ( query, result ) =
    Html.div [ Attr.style [ ( "padding-left", "10px" ) ] ]
        [ Html.p []
            [ Html.input [ Html.onInput QueryUpdated, Attr.value query ] [] ]
        , Html.p []
            [ Html.text result ]
        ]
