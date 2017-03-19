module Uhr exposing (..)

import Html
import Time exposing (Time)
import Time.DateTime as DT exposing (DateTime)
import Task


type alias Model =
    DateTime


type Message
    = Tick Time
    | NoOp


main : Program Never Model Message
main =
    Html.program
        { init = ( DT.dateTime DT.zero, systemZeit )
        , update = update
        , view = view
        , subscriptions = always jedeSekunde
        }


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        Tick zeit ->
            ( DT.fromTimestamp zeit, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html.Html Message
view time =
    Html.text (zeigeZeit time)


zeigeZeit : DateTime -> String
zeigeZeit time =
    toString (DT.hour time)
        ++ ":"
        ++ toString (DT.minute time)
        ++ ":"
        ++ toString (DT.second time)


jedeSekunde : Sub Message
jedeSekunde =
    Time.every Time.second Tick


systemZeit : Cmd Message
systemZeit =
    let
        onResult res =
            case res of
                Result.Ok time ->
                    Tick time

                Result.Err _ ->
                    NoOp
    in
        Task.attempt onResult Time.now
