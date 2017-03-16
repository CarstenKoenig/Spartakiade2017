{-# LANGUAGE OverloadedStrings #-}

module Countdown.AttemptsSpecs (main, spec) where

import Test.Hspec
import Countdown.Game (Challange (..), Attempt(..), attemptFromFormula)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "wenn ein Spieler einen Vorschlag einreicht" $ do
    let challange = Challange 765 [1,3,7,10,25,25,50]
    context "und dabei eine valide Formel nur mit den gegebenen Zahlen benutzt" $ do
      let playerAttempt = "7*25 + 10*50"
          attmpt = attemptFromFormula challange 1 playerAttempt
      it "wird die Formel uebernommen" $
        formula attmpt `shouldBe` playerAttempt
      it "wird der Wert gesetzt" $
        value attmpt `shouldBe` Just 675
      it "wird die Differenz zum Zielwert gebildet" $
        difference attmpt `shouldBe` Just 90
      it "ist die Info 'OK'" $
        info attmpt `shouldBe` "OK"
    context "und die Formel einen Syntaxfehler enthaelt" $ do
      let playerAttempt = "7*25 +"
          attmpt = attemptFromFormula challange 1 playerAttempt
      it "wird die Formel hier uebernommen" $
        formula attmpt `shouldBe` playerAttempt
      it "wird der Wert nicht gesetzt" $
        value attmpt `shouldBe` Nothing
      it "wird die Differenz nicht gesetzt" $
        difference attmpt `shouldBe` Nothing
      it "ist die Info 'Syntaxfehler in Formel'" $
        info attmpt `shouldBe` "Syntaxfehler in Formel"
    context "und die Formel nicht den Regeln entspricht (Teilterm negativ)" $ do
      let playerAttempt = "7*(3-10)"
          attmpt = attemptFromFormula challange 1 playerAttempt
      it "wird die Formel auch uebernommen" $
        formula attmpt `shouldBe` playerAttempt
      it "wird der Wert hier nicht gesetzt" $
        value attmpt `shouldBe` Nothing
      it "wird die Differenz nicht gesetzt" $
        difference attmpt `shouldBe` Nothing
      it "ist die Info 'Formel enthaelt ungueltige Terme'" $
        info attmpt `shouldBe` "Formel enthaelt ungueltige Terme"
    context "die Formel nicht den Regeln entspricht (Teilen durch 0)" $ do
      let playerAttempt = "7/(25-25)"
          attmpt = attemptFromFormula challange 1 playerAttempt
      it "wird die Formel auch uebernommen" $
        formula attmpt `shouldBe` playerAttempt
      it "wird der Wert nicht gesetzt" $
        value attmpt `shouldBe` Nothing
      it "wird die Differenz nicht gesetzt" $
        difference attmpt `shouldBe` Nothing
      it "ist die Info 'Formel enthaelt ungueltige Terme'" $
        info attmpt `shouldBe` "Formel enthaelt ungueltige Terme"
    context "und die Formel nicht vorgegebene Zahlen enthaelt" $ do
      let playerAttempt = "7*5"
          attmpt = attemptFromFormula challange 1 playerAttempt
      it "wird die Formel wieder uebernommen" $
        formula attmpt `shouldBe` playerAttempt
      it "wird der Wert hier nicht gesetzt" $
        value attmpt `shouldBe` Nothing
      it "wird die Differenz nicht gesetzt" $
        difference attmpt `shouldBe` Nothing
      it "ist die Info 'Formel darf nur die gegebenen Zahlen verwenden'" $
        info attmpt `shouldBe` "Formel darf nur die gegebenen Zahlen verwenden"
    context "und die Formel vorgegebene Zahlen zu oft enthaelt" $ do
      let playerAttempt = "25+25*25"
          attmpt = attemptFromFormula challange 1 playerAttempt
      it "wird die Formel uebernommen" $
        formula attmpt `shouldBe` playerAttempt
      it "wird der Wert nicht gesetzt" $
        value attmpt `shouldBe` Nothing
      it "wird die Differenz nicht gesetzt" $
        difference attmpt `shouldBe` Nothing
      it "ist die Info 'Formel darf nur die gegebenen Zahlen verwenden'" $
        info attmpt `shouldBe` "Formel darf nur die gegebenen Zahlen verwenden"
        
