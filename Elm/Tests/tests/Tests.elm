module Tests exposing (..)

import Test exposing (..)
import Expect
import Json.Decode as Json
import Komm exposing (..)


testJsonResponse : String
testJsonResponse =
    "{\"runde\":{\"herausforderung\":{\"targetNumber\":100,\"availableNumbers\":[2,3,4,5,6]},\"versuche\":[{\"wert\":90,\"punkte\":5,\"spieler\":{\"nickName\":\"Spieler1\"}},{\"wert\":null,\"punkte\":0,\"spieler\":{\"nickName\":\"Spiele2r\"}}]},\"naechsteRundeUm\":\"2017-03-16T16:52:34.932921Z\"}"


all : Test
all =
    describe "Json Decoder"
        [ describe "für eine vollständige Antwort"
            [ test "enthält Runde" <|
                checkSpielzustand <|
                    \zustand ->
                        case zustand.runde of
                            Just _ ->
                                Expect.pass

                            Nothing ->
                                Expect.fail "keine Runde dekodiert"
            , test "hat Herausforderung mit ZielZahl 100" <|
                checkHerausforderung <|
                    \herausforderung -> Expect.equal 100 herausforderung.zielZahl
            ]
        ]



--- Hilfsfunktionen


testSpielZustand : Result String SpielZustand
testSpielZustand =
    Json.decodeString decodeZustand testJsonResponse


checkSpielzustand : (SpielZustand -> Expect.Expectation) -> () -> Expect.Expectation
checkSpielzustand check () =
    case testSpielZustand of
        Result.Ok zustand ->
            check zustand

        Result.Err err ->
            Expect.fail (toString err)


checkRunde : (SpielRunde -> Expect.Expectation) -> () -> Expect.Expectation
checkRunde check =
    checkSpielzustand
        (\zustand ->
            case zustand.runde of
                Just runde ->
                    check runde

                Nothing ->
                    Expect.fail "dekodierter Zustand enthält keine Runde"
        )


checkHerausforderung : (Herausforderung -> Expect.Expectation) -> () -> Expect.Expectation
checkHerausforderung check =
    checkRunde
        (\runde -> check runde.herausforderung)
