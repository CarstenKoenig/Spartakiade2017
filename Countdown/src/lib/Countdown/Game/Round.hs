{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Countdown.Game.Round
       ( Attempt (..)
       , RoundT (..)
       , RoundState (..)
       , playerAttempt
       , listPlayers
       , newRandomRound
       )where

import GHC.Generics (Generic)

import Control.Monad.State.Strict (StateT(..))
import qualified Control.Monad.State.Strict as State

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Countdown.Expressions (values, eval)
import Countdown.Parser (tryParse)
import Countdown.Lists (isSubsetOf)

import Countdown.Game.Players (PlayerId, Player)
import Countdown.Game.Challanges (Challange(..))
import Countdown.Game.Attempts (Attempt(..), attemptFromFormula)
import Countdown.Game.Random (generateChallange)


type RoundT m a = StateT RoundState m a


data RoundState =
  RoundState
  { roundPlayers :: [PlayerId]
  , roundAttempts :: Map PlayerId Attempt
  , roundChallange :: Challange 
  } deriving (Generic, Show)

instance ToJSON RoundState


playerAttempt :: Monad m => PlayerId -> Text -> RoundT m Attempt
playerAttempt pId txt = do
  state <- State.get
  let att = attemptFromFormula (roundChallange state) pId txt
  State.put $
    state { roundAttempts = M.insert pId att (roundAttempts state) }
  return att


listPlayers :: Monad m => RoundT m [PlayerId]
listPlayers =
  roundPlayers <$> State.get


newRandomRound :: [PlayerId] -> IO RoundState
newRandomRound pls = do
  ch <- generateChallange
  return $ RoundState pls M.empty ch
