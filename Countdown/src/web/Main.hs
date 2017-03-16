{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Prelude hiding (readFile)

import Web.Spock
import Web.Spock.Config

import Control.Concurrent (ThreadId, threadDelay, forkIO)
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar

import Control.Exception.Base (AsyncException, handle, fromException, throwIO)
import Control.Monad (forever, (>=>))
import Control.Monad.State.Strict (runStateT)
import Control.Monad.Trans


import qualified Data.Aeson as Json
import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Time

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

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

data MySession =
  MySession
  { player :: Maybe Player
  , playerId :: Maybe PlayerId
  }


data MyAppState =
  MyAppState
  { gameState    :: MVar GameState
  , players      :: MVar Game.PlayersMap
  , nextPlayerId :: MVar PlayerId
  , wsChannel    :: Chan ZustandDto
  }


data GameState
  = WaitForNextRound UTCTime
  | PlayingRound UTCTime Game.RoundState


data ZustandDto =
  ZustandDto
  { runde :: Maybe RundeDto
  , naechsteRundeUm :: UTCTime
  } deriving (Generic, Show)

instance ToJSON ZustandDto


data RundeDto =
  RundeDto
  { versuche :: [Versuch]
  , herausforderung :: Game.Challange
  } deriving (Generic, Show)

instance ToJSON RundeDto


data Versuch =
  Versuch
  { wert    :: Maybe Int
  , punkte  :: Int
  , spieler :: Game.Player
  } deriving (Generic, Show)

instance ToJSON Versuch


example :: IO ZustandDto
example = do
  nxt <- Time.addUTCTime 60 <$> Time.getCurrentTime
  return $ ZustandDto (Just runde) nxt
  where
    runde = RundeDto [versuch1, versuch2] heraus
    heraus = Game.Challange 100 [2,3,4,5,6]
    versuch1 = Versuch (Just 90) 5 (Game.Player "Spieler1")
    versuch2 = Versuch Nothing 0 (Game.Player "Spiele2r")
    

gameLoop :: Chan ZustandDto -> MVar Game.PlayersMap -> MVar GameState -> IO ThreadId
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
        Chan.writeChan chan $ zustandDto ps (PlayingRound till rs)
    PlayingRound till rs
      | now >= till -> do
         at <- MVar.modifyMVar gsv
           (\ _ -> do
             at <- Time.addUTCTime 60 <$> Time.getCurrentTime
             return (WaitForNextRound at, at))
         Chan.writeChan chan $ zustandDto ps (WaitForNextRound at)
    _ ->
      threadDelay 1000000

      
zustandDto :: Game.PlayersMap -> GameState -> ZustandDto
zustandDto psm gs = ZustandDto aktRunde (till gs)
  where
    aktRunde = rundeAus <$> roundSt gs
    roundSt (WaitForNextRound _) = Nothing
    roundSt (PlayingRound _ rs) = Just rs
    till (WaitForNextRound t) = t
    till (PlayingRound t _) = t
    rundeAus roundState = RundeDto
      (Map.elems $  Map.map versuchAus (Game.roundAttempts roundState))
      (Game.roundChallange roundState)
    spieler att =
      fromJust $ Players.lookup (Game.fromPlayer att) psm
    versuchAus att = Versuch
      (Game.value att)
      (Game.score att)
      (spieler att)


aktuellerZustand :: MyAppState -> IO ZustandDto
aktuellerZustand state = do
  ps <- MVar.readMVar (players state)
  fmap (zustandDto ps) . MVar.readMVar $ gameState state


spielerVersuch :: Game.PlayerId -> Text -> SpockActionCtx () conn MySession MyAppState (Maybe Game.Attempt)
spielerVersuch pId txt = do
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
  liftIO $ maybe (return ()) (Chan.writeChan chan . zustandDto ps) gs
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


registerPlayer :: PlayerId -> Text ->  SpockActionCtx () conn MySession MyAppState Player
registerPlayer pId nick = do
  state <- getState
  pmap <- liftIO $ MVar.takeMVar (players state)
  let (pmap',pl) = Players.update pId nick pmap
  liftIO $ MVar.putMVar (players state) pmap'
  writeSession (MySession (Just pl) (Just pId))
  return pl


main :: IO ()
main = do
  chan <- Chan.newChan
  cfg <- spockCfg chan
  app <- mainApp chan cfg
  Warp.run 8090 app
  

mainApp :: Chan ZustandDto -> SpockCfg () MySession MyAppState -> IO Application
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
    state <- getState
    runde <- liftIO $ aktuellerZustand state
    json runde

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
        att <- spielerVersuch pId formula
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
        
        

spockCfg :: Chan ZustandDto -> IO (SpockCfg () MySession MyAppState)
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


websocketLoop :: Ws.Connection -> Chan ZustandDto -> IO ()
websocketLoop conn chan = do
  Ws.forkPingThread conn 3 
  forever $ do
    runde <- Chan.readChan chan
    Ws.sendTextData conn (Json.encode runde)
