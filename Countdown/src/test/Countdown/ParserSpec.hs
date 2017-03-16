{-# LANGUAGE OverloadedStrings #-}

module Countdown.ParserSpec (main, spec) where

import Test.Hspec
import Countdown.Parser
import Countdown.Expressions

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "beim Evaluieren eines mit tryParse gelesenen Textes" $ do
    context "einfache Zahlen" $ do
      it "ergibt '0' [] da 0 nicht positiv ist" $ do
        (eval <$> tryParse "0") `shouldBe` Just []
      it "ergibt '5' einfach 5" $ do
        (eval <$> tryParse "5") `shouldBe` Just [5]
      it "ergibt '99' einfach 99" $ do
        (eval <$> tryParse "99") `shouldBe` Just [99]
    context "einfache Zahlen mit Leerzeichen" $ do
      it "' 2' ist 2" $ do
        (eval <$> tryParse " 2") `shouldBe` Just [2]
      it "'2 ' ist 2" $ do
        (eval <$> tryParse "2 ") `shouldBe` Just [2]
      it "' 2 ' ist 2" $ do
        (eval <$> tryParse " 2 ") `shouldBe` Just [2]
    context "Multiplikation" $ do
      it "2*3 = 6" $ do
        (eval <$> tryParse "2*3") `shouldBe` Just [6]
      it "2*3*4 = 24" $ do
        (eval <$> tryParse "2*3*4") `shouldBe` Just [24]
    context "Division" $ do
      it "8/2 = 4" $ do
        (eval <$> tryParse "8/2") `shouldBe` Just [4]
      it "8/2/2 = 2" $ do
        (eval <$> tryParse "8/2/2") `shouldBe` Just [2]
    context "Multiplikation und Division mit Leerzeichen" $ do
      it "6 *2 / 3 = 4" $ do
        (eval <$> tryParse "6 *2 / 3") `shouldBe` Just [4]
    context "Addition" $ do
      it "2+3 = 5" $ do
        (eval <$> tryParse "2+3") `shouldBe` Just [5]
      it "2+3+4 = 9" $ do
        (eval <$> tryParse "2+3+4") `shouldBe` Just [9]
    context "Subtraktion" $ do
      it "8-2 = 6" $ do
        (eval <$> tryParse "8-2") `shouldBe` Just [6]
      it "8-2-2 = 4" $ do
        (eval <$> tryParse "8-2-2") `shouldBe` Just [4]
    context "Punkt-vor-Strich" $ do
      it "3+2*2 = 7" $ do
        (eval <$> tryParse "3+2*2") `shouldBe` Just [7]
    context "Klammern" $ do
      it "(3+2)*2 = 10" $ do
        (eval <$> tryParse "(3+2)*2") `shouldBe` Just [10]
      it " (3+2)*2 = 10" $ do
        (eval <$> tryParse " (3+2)*2") `shouldBe` Just [10]
    context "Komplexe Formel" $ do
      it "(8 - (16 / (6- 2))- 1*2) + 3" $ do
        (eval <$> tryParse "(8 - (16 / (6- 2))- 1*2) + 3") `shouldBe` Just [5]
        
