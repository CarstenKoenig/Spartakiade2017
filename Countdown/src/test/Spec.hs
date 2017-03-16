{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import qualified Countdown.ParserSpec as Parser
import qualified Countdown.AttemptsSpecs as Attempts


main :: IO ()
main = hspec $ do
  Parser.spec
  Attempts.spec
