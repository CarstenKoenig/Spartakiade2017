module Kommunikation
    exposing
        ( SpielRunde
        , SpielZustand
        , Herausforderung
        , Versuch
        , EigenerVersuch
        , Spieler
        , initZustand
        , getSpielZustand
        , getLogin
        , postLogin
        , postVersuch
        , websocketSub
        )

import Json.Decode as Json exposing (Decoder)
import Http as Http exposing (Request, Error(..))
import Time.ZonedDateTime as Time exposing (ZonedDateTime)
import Time.TimeZones exposing (europe_berlin)
import WebSocket exposing (listen)


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


type alias EigenerVersuch =
    { wert : Maybe Int
    , punkte : Int
    }


type alias Spieler =
    { nick : String
    }


initZustand : SpielZustand
initZustand =
    { runde = Nothing
    , endetUm = Time.zonedDateTime (europe_berlin ()) Time.zero
    }



-- GET Eingeloggter Spieler


getLogin : (Error -> msg) -> (Spieler -> msg) -> Cmd msg
getLogin =
    getCmd "/me" decodeSpieler



-- POST Einloggen


postLogin : String -> (Error -> msg) -> (Spieler -> msg) -> Cmd msg
postLogin nickName =
    let
        body =
            Http.multipartBody [ Http.stringPart "name" nickName ]
    in
        postCmd "/login" body decodeSpieler



-- POST Versuch


postVersuch : String -> (Error -> msg) -> (Result String EigenerVersuch -> msg) -> Cmd msg
postVersuch formel =
    let
        body =
            Http.multipartBody [ Http.stringPart "formula" formel ]
    in
        postCmd "/attempt" body decodeEigenerVersuch



-- GET Zustand


getSpielZustand : (Error -> msg) -> (SpielZustand -> msg) -> Cmd msg
getSpielZustand =
    getCmd "/runde" decodeZustand



-- abstrakter AJAX GET:


getCmd : String -> Decoder res -> (Error -> msg) -> (res -> msg) -> Cmd msg
getCmd url decoder onError onOk =
    let
        request =
            Http.get url decoder

        mapResult res =
            case res of
                Err err ->
                    onError err

                Ok res ->
                    onOk res
    in
        Http.send mapResult request



-- abstrakter AJAX POST:


postCmd : String -> Http.Body -> Decoder res -> (Error -> msg) -> (res -> msg) -> Cmd msg
postCmd url body decoder onError onOk =
    let
        request =
            Http.post url body decoder

        mapResult res =
            case res of
                Err err ->
                    onError err

                Ok res ->
                    onOk res
    in
        Http.send mapResult request



-- Websocket


websocketSub : String -> (String -> msg) -> (SpielZustand -> msg) -> Sub msg
websocketSub socketUrl onFehler onZustand =
    let
        parse inp =
            case Json.decodeString decodeZustand inp of
                Ok value ->
                    onZustand value

                Err err ->
                    onFehler err
    in
        listen socketUrl parse



-- Deokodieren des Zustands
-- Beispiel:
-- {"runde":{"herausforderung":{"targetNumber":596,"availableNumbers":[5,5,6,8,15,75]},"versuche":{},"restDauerSek":219},"naechsteRundeUm":"2017-03-14T07:07:27.9060232Z"}


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



-- Dekodieren der Runde
-- Beispiel:
-- {"herausforderung":{"targetNumber":369,"availableNumbers":[1,2,3,9,15,25]},"versuche":{}}


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



-- Beispiel
-- {... "versuche":[{"wert":100,"punkte":5,"spieler":{"nickName":"Carsten"}}]},...}


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



-- Beispiel:
-- {"fromPlayer":1,"score":0,"value":105,"formula":"7 * 15","difference":40,"info":"OK"}


decodeEigenerVersuch : Decoder (Result String EigenerVersuch)
decodeEigenerVersuch =
    let
        decodeWert =
            Json.at [ "value" ] (Json.maybe Json.int)

        decodePunkte =
            Json.at [ "score" ] Json.int

        decodeInfo =
            Json.at [ "info" ] Json.string

        decide info wert punkte =
            case info of
                "OK" ->
                    Result.Ok (EigenerVersuch wert punkte)

                _ ->
                    Result.Err info
    in
        Json.map3 decide decodeInfo decodeWert decodePunkte


decodeSpieler : Decoder Spieler
decodeSpieler =
    let
        decodeNick =
            Json.at [ "nickName" ] Json.string
    in
        Json.map Spieler decodeNick
