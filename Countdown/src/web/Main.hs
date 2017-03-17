{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile)

import Web.Spock
import Web.Spock.Config

import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar

import Control.Exception.Base (AsyncException, handle, fromException, throwIO)
import Control.Monad (forever)
import Control.Monad.State.Strict (runStateT)
import Control.Monad.Trans


import qualified Data.Aeson as Json
import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Time

import qualified Network.HTTP.Types.Status as HttpStatus
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai (Middleware, Application)
import Network.Wai.Middleware.Static (staticPolicy, addBase)

import Network.WebSockets (ServerApp)
import qualified Network.WebSockets as Ws
import qualified Network.Wai.Handler.WebSockets as Wsh

import qualified Countdown.Game as Game
import Countdown.Game.Players (Player, PlayerId)
import qualified Countdown.Game.Players as Players

import AppState
import GameLoop

main :: IO ()
main = do
  chan <- Chan.newChan
  cfg <- spockCfg chan
  app <- mainApp chan cfg
  Warp.run 8090 app
  

mainApp :: Chan GameState -> SpockCfg () MySession MyAppState -> IO Application
mainApp chan cfg = do
  spApp <- spockAsApp $ spock cfg spockApp
  return $ Wsh.websocketsOr Ws.defaultConnectionOptions wsApp spApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
      conn <- Ws.acceptRequest pending_conn
      websocketLoop conn chan


spockApp :: SpockM () MySession MyAppState ()
spockApp = do

  -- serve static files from local static folder
  middleware serveStatic
  
  get root $ do
    page <- liftIO indexPage
    html page

  get "runde" $ do
    game <- getCurrentGameState
    json game

  post "login" $ do
    name <- param "name"
    case name of
      Just name -> do
        pId <- currentPlayerId
        player <- registerPlayer pId name
        json player
      Nothing ->
        setStatus HttpStatus.noContent204

  post "attempt" $ do
    (MySession _ meId) <- readSession
    formula <- param "formula"
    case (formula, meId) of
      (Just formula, Just pId) -> do
        att <- registerPlayerAttempt pId formula
        maybe (setStatus HttpStatus.badRequest400) json att
      _ ->
        setStatus HttpStatus.badRequest400
        

  get "me" $ do
    (MySession me _) <- readSession
    case me of
      Just player ->
        json player
      Nothing ->
        setStatus HttpStatus.noContent204

  get "example" $ do
    ex <- liftIO example
    json ex

      
spockCfg :: Chan GameState -> IO (SpockCfg () MySession MyAppState)
spockCfg chan = do
  startAt <- Time.addUTCTime 60 <$> Time.getCurrentTime
  gst <- MVar.newMVar (WaitForNextRound startAt)
  pmap <- MVar.newMVar Map.empty
  nId <- MVar.newMVar 1
  cfg <- defaultSpockCfg
         (MySession Nothing Nothing)
         PCNoDatabase
         (MyAppState gst pmap nId chan)
  gameLoop chan pmap gst
  pure $
    cfg { spc_sessionCfg =
          (spc_sessionCfg cfg) { sc_cookieName = "CountdownGame" }
        }
      

indexPage :: IO Text
indexPage = readFile "./static/index.html"


serveStatic :: Middleware
serveStatic = staticPolicy (addBase "./static")


websocketLoop :: Ws.Connection -> Chan GameState -> IO ()
websocketLoop conn chan = do
  Ws.forkPingThread conn 3 
  forever $ do
    runde <- Chan.readChan chan
    Ws.sendTextData conn (Json.encode runde)
