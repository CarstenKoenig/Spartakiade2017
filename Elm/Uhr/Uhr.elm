module Uhr exposing (..)

import Html exposing (Html)
import Time exposing (Time)
import Time.DateTime as DT exposing (DateTime)
import Task


type alias Model =
    DateTime


type Message
    = Tick Time
    | NoOp


main : Program Never DateTime Message
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
        NoOp ->
            model ! []

        Tick zeit ->
            DT.fromTimestamp zeit ! []


view : Model -> Html Message
view time =
    Html.text (zeigeZeit time)


systemZeit : Cmd Message
systemZeit =
    let
        onResult res =
            case res of
                Result.Err fehler ->
                    Debug.log (toString fehler) NoOp

                Result.Ok zeit ->
                    Tick zeit
    in
        Time.now
            |> Task.attempt onResult


jedeSekunde : Sub Message
jedeSekunde =
    Time.every Time.second Tick


zeigeZeit : DateTime -> String
zeigeZeit zeit =
    let
        zweiStellig getter =
            let
                zeitString =
                    toString (getter zeit)
            in
                case String.length zeitString of
                    0 ->
                        "00"

                    1 ->
                        "0" ++ zeitString

                    _ ->
                        zeitString
    in
        zweiStellig DT.hour
            ++ ":"
            ++ zweiStellig DT.minute
            ++ ":"
            ++ zweiStellig DT.second
