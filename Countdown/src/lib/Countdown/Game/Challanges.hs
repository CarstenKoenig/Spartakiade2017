{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Countdown.Game.Challanges
       ( Challange (..)
       )where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON)

data Challange =
  Challange
  { targetNumber     :: Int
  , availableNumbers :: [Int]
  } deriving (Generic, Show)

instance ToJSON Challange
