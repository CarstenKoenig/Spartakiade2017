{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module AppState
  ( MyAppState (..)
  , MySession (..)
  , GamePhase (..)
  , GameState
  , getCurrentGameState
  , getGameState
  , currentPlayerId
  , registerPlayer
  , registerPlayerAttempt
  , example
  ) where

import Web.Spock (SpockActionCtx, getState, writeSession, readSession)

import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan

import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar

import Control.Monad.Trans (liftIO)
import Control.Monad.State.Strict (runStateT)

import Data.Aeson (ToJSON(..))
import qualified Data.Aeson as Json
import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Time

import GHC.Generics (Generic)

import qualified Countdown.Game as Game
import Countdown.Game.Players (Player, PlayerId)
import qualified Countdown.Game.Players as Players


data MySession =
  MySession
  { player :: Maybe Player
  , playerId :: Maybe PlayerId
  }


data MyAppState =
  MyAppState
  { gameState    :: MVar GamePhase
  , players      :: MVar Game.PlayersMap
  , nextPlayerId :: MVar PlayerId
  , wsChannel    :: Chan GameState
  }


data GamePhase
  = WaitForNextRound UTCTime
  | PlayingRound UTCTime Game.RoundState


registerPlayerAttempt :: Game.PlayerId -> Text
                      -> SpockActionCtx () conn MySession MyAppState (Maybe Game.Attempt)
registerPlayerAttempt pId txt = do
  state <- getState
  ps <- liftIO $ MVar.readMVar (players state)
  let chan = wsChannel state
  (gs, att) <- liftIO $ MVar.modifyMVar
    (gameState state)
    (\gs ->
        case gs of
          WaitForNextRound _ ->
            return (gs, (Nothing, Nothing))
          PlayingRound t rs -> do
            (att, rs') <- runStateT (Game.playerAttempt pId txt) rs
            let gs' = PlayingRound t rs'
            return (gs', (Just gs', Just att)))
  liftIO $ maybe (return ()) (Chan.writeChan chan . getGameState ps) gs
  return att


currentPlayerId :: SpockActionCtx () conn MySession MyAppState PlayerId
currentPlayerId = do
  (MySession _ opt) <- readSession
  case opt of
    Just pId ->
      return pId
    Nothing -> do
      state <- getState
      let var = nextPlayerId state
      nId <- liftIO $ MVar.takeMVar var
      liftIO $ MVar.putMVar var (nId + 1)
      writeSession (MySession Nothing (Just nId))
      return nId


registerPlayer :: PlayerId -> Text
               -> SpockActionCtx () conn MySession MyAppState Player
registerPlayer pId nick = do
  state <- getState
  pmap <- liftIO $ MVar.takeMVar (players state)
  let (pmap',pl) = Players.update pId nick pmap
  liftIO $ MVar.putMVar (players state) pmap'
  writeSession (MySession (Just pl) (Just pId))
  return pl


getCurrentGameState :: SpockActionCtx () conn MySession MyAppState GameState
getCurrentGameState = do
  state <- getState
  ps <- liftIO $ MVar.readMVar (players state)
  liftIO $ fmap (getGameState ps) . MVar.readMVar $ gameState state


data GameState =
  GameState
  { runde :: Maybe GameRound
  , naechsteRundeUm :: UTCTime
  } deriving (Generic, Show)

instance ToJSON GameState


data GameRound =
  GameRound
  { versuche :: [Versuch]
  , herausforderung :: Game.Challange
  } deriving (Generic, Show)

instance ToJSON GameRound


data Versuch =
  Versuch
  { wert    :: Maybe Int
  , punkte  :: Int
  , spieler :: Game.Player
  } deriving (Generic, Show)

instance ToJSON Versuch


example :: IO GameState
example = do
  nxt <- Time.addUTCTime 60 <$> Time.getCurrentTime
  return $ GameState (Just runde) nxt
  where
    runde = GameRound [versuch1, versuch2] heraus
    heraus = Game.Challange 100 [2,3,4,5,6]
    versuch1 = Versuch (Just 90) 5 (Game.Player "Spieler1")
    versuch2 = Versuch Nothing 0 (Game.Player "Spiele2r")
    

      
getGameState :: Game.PlayersMap -> GamePhase -> GameState
getGameState psm gs = GameState aktRunde (till gs)
  where
    aktRunde = rundeAus <$> roundSt gs
    roundSt (WaitForNextRound _) = Nothing
    roundSt (PlayingRound _ rs) = Just rs
    till (WaitForNextRound t) = t
    till (PlayingRound t _) = t
    rundeAus roundState = GameRound
      (Map.elems $  Map.map versuchAus (Game.roundAttempts roundState))
      (Game.roundChallange roundState)
    spieler att =
      fromJust $ Players.lookup (Game.fromPlayer att) psm
    versuchAus att = Versuch
      (Game.value att)
      (Game.score att)
      (spieler att)

