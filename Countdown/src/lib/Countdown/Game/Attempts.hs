{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Countdown.Game.Attempts
       ( Attempt (..)
       , AttemptsMap
       , attempt
       , attemptFromFormula
       )where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Countdown.Expressions (values, eval)
import Countdown.Parser (tryParse)
import Countdown.Lists (isSubsetOf)

import Countdown.Game.Players (PlayerId, Player)
import Countdown.Game.Challanges (Challange(..))

type AttemptsMap = Map PlayerId Attempt

data Attempt =
  Attempt
  { formula    :: Text
  , value      :: Maybe Int
  , difference :: Maybe Int
  , score      :: Int
  , fromPlayer :: PlayerId
  , info       :: Text
  } deriving (Generic, Show)

instance ToJSON Attempt

attempt :: Challange -> Text -> PlayerId -> AttemptsMap -> (AttemptsMap, Attempt)
attempt ch txt pId aMap =
  let att = attemptFromFormula ch pId txt
  in (M.insert pId att aMap, att)

attemptFromFormula :: Challange -> PlayerId -> Text -> Attempt
attemptFromFormula ch pId txt =
  case tryParse txt of
    Nothing -> Attempt txt Nothing Nothing 0 pId "Syntaxfehler in Formel"
    Just ex -> if values ex `isSubsetOf` availableNumbers ch
               then mapValue $ eval ex
               else Attempt txt Nothing Nothing 0 pId "Formel darf nur die gegebenen Zahlen verwenden"
  where
    mapValue []  = Attempt txt Nothing Nothing 0 pId "Formel enthaelt ungueltige Terme"
    mapValue [v] = Attempt txt (Just v) (Just $ dif v) (score' $ dif v) pId "OK"
    mapValue _   = error "kein eindeutiges Ergebnis"
    dif v' = abs (targetNumber ch - v')
    score' d
      | d == 0     = 10
      | d <= 5     = 7
      | d <= 10    = 5 
      | otherwise = 0
