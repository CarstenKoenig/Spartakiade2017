module Komm exposing (..)

import Json.Decode as Json exposing (Decoder)
import Time.ZonedDateTime as Time exposing (ZonedDateTime)
import Time.TimeZones exposing (europe_berlin)


type alias SpielZustand =
    { runde : Maybe SpielRunde
    , endetUm : ZonedDateTime
    }


type alias SpielRunde =
    { herausforderung : Herausforderung
    , versuche : List Versuch
    }


type alias Herausforderung =
    { zielZahl : Int
    , zahlenvorat : List Int
    }


type alias Versuch =
    { wert : Maybe Int
    , punkte : Int
    , spieler : Spieler
    }


type alias Spieler =
    { nick : String
    }


decodeZustand : Decoder SpielZustand
decodeZustand =
    Json.map2 SpielZustand
        (Json.at [ "runde" ] (Json.nullable decodeRunde))
        (Json.at [ "naechsteRundeUm" ] decodeUtc)


decodeUtc : Decoder ZonedDateTime
decodeUtc =
    let
        parseUtc inp =
            Time.fromISO8601 (europe_berlin ()) inp
                |> Result.withDefault (Time.zonedDateTime (europe_berlin ()) Time.zero)
    in
        Json.map parseUtc Json.string


decodeRunde : Decoder SpielRunde
decodeRunde =
    Json.map2 SpielRunde
        (Json.at [ "herausforderung" ] decodeHerausforderung)
        (Json.at [ "versuche" ] decodeVersuche)


decodeHerausforderung : Decoder Herausforderung
decodeHerausforderung =
    let
        decodeZahl =
            Json.at [ "targetNumber" ] Json.int

        decodeZahlen =
            Json.at [ "availableNumbers" ] (Json.list Json.int)
    in
        Json.map2 Herausforderung decodeZahl decodeZahlen


decodeVersuche : Decoder (List Versuch)
decodeVersuche =
    Json.list decodeVersuch


decodeVersuch : Decoder Versuch
decodeVersuch =
    let
        decodeWert =
            Json.at [ "wert" ] (Json.maybe Json.int)

        decodePunkte =
            Json.at [ "punkte" ] Json.int

        decodeSp =
            Json.at [ "spieler" ] decodeSpieler
    in
        Json.map3 Versuch decodeWert decodePunkte decodeSp


decodeSpieler : Decoder Spieler
decodeSpieler =
    let
        decodeNick =
            Json.at [ "nickName" ] Json.string
    in
        Json.map Spieler decodeNick
