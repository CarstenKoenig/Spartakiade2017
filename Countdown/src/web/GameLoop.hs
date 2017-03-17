module GameLoop (gameLoop) where

import Control.Concurrent (ThreadId, threadDelay, forkIO)
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan

import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Control.Monad (forever)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Time

import qualified Countdown.Game as Game
import Countdown.Game.Players (Player, PlayerId)
import qualified Countdown.Game.Players as Players

import AppState


gameLoop :: Chan GameState -> MVar Game.PlayersMap -> MVar GamePhase -> IO ThreadId
gameLoop chan psv gsv = forkIO $ forever $ do
  gs <- MVar.readMVar gsv
  now <- Time.getCurrentTime
  ps <- MVar.readMVar psv
  case gs of
    WaitForNextRound at
      | now >= at -> do
        (till,rs) <- MVar.modifyMVar gsv
          (\ _ -> do
              round <- Game.newRandomRound (Map.keys ps)
              till <- Time.addUTCTime 300 <$> Time.getCurrentTime
              return (PlayingRound till round, (till, round)))
        Chan.writeChan chan $ getGameState ps (PlayingRound till rs)
    PlayingRound till rs
      | now >= till -> do
         at <- MVar.modifyMVar gsv
           (\ _ -> do
             at <- Time.addUTCTime 60 <$> Time.getCurrentTime
             return (WaitForNextRound at, at))
         Chan.writeChan chan $ getGameState ps (WaitForNextRound at)
    _ ->
      threadDelay 1000000
        

