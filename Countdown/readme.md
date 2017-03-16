# Countdown

## Link-Samlung
- [Haskell-Spock](https://www.stackage.org/haddock/lts-8.5/Spock-0.12.0.0/Web-Spock.html)
- [Haskell-Spock-Core](https://www.stackage.org/haddock/lts-8.5/Spock-core-0.12.0.0/Web-Spock-Core.html)
- [Elm-Bootstrap](http://elm-bootstrap.info/card)
- [Elm-Bootstrap API](http://package.elm-lang.org/packages/rundis/elm-bootstrap/latest)
- [Elm-Http](http://package.elm-lang.org/packages/elm-lang/http/1.0.0/Http)
- [Elm-Json](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Json-Decode)

## Elm - Client
In einem neuen Verzeichnis:

	mkdir client
	cd client
	elm make
	
damit bereitet Elm das Verzeichnis vor

### Zusätzliche Packages

	    "elm-lang/http"
        "elm-lang/websocket"
        "rundis/elm-bootstrap"
		
jeweils mit `elm package install` hinzufügen	

### Test-App mit Bootstrap:

```elm
module Main exposing (main)

import Platform.Sub as Subs
import Html exposing (Html, div, text, button)
import Html.Attributes as Attr
import Bootstrap.Grid as Grid
import Bootstrap.CDN as CDN


type alias Model =
    ()


type alias Flags =
    { websocketUrl : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( ()
    , Cmd.none
    )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            () ! [ Cmd.none ]


view : Model -> Html Msg
view () =
    Grid.container []
        [ CDN.stylesheet
          -- creates an inline style node with the Bootstrap CSS
        , div [ Attr.class "jumbotron" ] [ Html.h1 [] [ text "Hallo Elm " ] ]
        , Grid.row []
            [ Grid.col []
                [ text "Some content for my view here..." ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Subs.none


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }
```

### JS erzeugen

    elm make main.elm --output ..\static\client.js
	
/newpage


### Runde dekodieren
über `/runde` **JSON** dekodieren:

#### Model/Dekoder
```elm
module Runde
    exposing
        ( SpielRunde
        , Herausforderung
        , Versuch
        , getSpielRunde
        , decodeRunde
        )

import Json.Decode as Json exposing (Decoder)
import Http as Http exposing (Request, Error(..))
import Task as Task exposing (Task)


type alias SpielRunde =
    { herausforderung : Herausforderung
    , versuche : List Versuch
    }


type alias Herausforderung =
    { zielZahl : Int
    , zahlenvorat : List Int
    }


type Versuch
    = Unit


getSpielRunde : (Error -> msg) -> (SpielRunde -> msg) -> Cmd msg
getSpielRunde onError onOk =
    Http.get "/runde" decodeRunde
        |> Http.toTask
        |> Task.attempt
            (\res ->
                case res of
                    Err err ->
                        onError err

                    Ok res ->
                        onOk res
            )



-- Dekodieren der Runde
-- Beispiel:
-- {"herausforderung":{"targetNumber":369,"availableNumbers":[1,2,3,9,15,25]},"versuche":{}}


decodeRunde : Decoder SpielRunde
decodeRunde =
    Json.map2 SpielRunde
        (Json.at [ "herausforderung" ] decodeHerausforderung)
        (Json.at [ "versuche" ] decodeVersuche)


decodeHerausforderung : Decoder Herausforderung
decodeHerausforderung =
    let
        decodeZahl =
            Json.at [ "targetNumber" ] Json.int

        decodeZahlen =
            Json.at [ "availableNumbers" ] (Json.list Json.int)
    in
        Json.map2 Herausforderung decodeZahl decodeZahlen


decodeVersuche : Decoder (List Versuch)
decodeVersuche =
    Json.succeed []
```

#### Darstellen
```elm
init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Nothing
    , (Runde.getSpielRunde HttpError UpdateRunde)
    )


type Msg
    = HttpError Error
    | UpdateRunde SpielRunde


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        HttpError _ ->
            model ! [ Cmd.none ]

        UpdateRunde runde ->
            (Just runde) ! [ Cmd.none ]


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
          -- creates an inline style node with the Bootstrap CSS
        , div [ Attr.class "jumbotron" ] [ Html.h1 [] [ text "Hallo Elm " ] ]
        , Grid.row []
            [ Grid.col []
                [ Maybe.map viewRunde model
                    |> Maybe.withDefault (text "---")
                ]
            ]
        ]


viewRunde : SpielRunde -> Html Msg
viewRunde runde =
    Card.config []
        |> Card.header []
            [ text "Herausforderung: "
            , Html.strong [] [ text (toString runde.herausforderung.zielZahl) ]
            ]
        |> Card.block []
            [ Card.titleH5 [] [ text "Zahlenvorat" ]
            , Card.text []
                [ Html.ul []
                    (runde.herausforderung.zahlenvorat
                        |> List.map (toString >> text >> List.singleton >> Html.li [])
                    )
                ]
            ]
        |> Card.view
```


## Haskell

### Spock Webapplikation

    stack new Countdown spock
	cd Countdown
	
folgende Packete brauchen wir (`Countdown.cabal`) zusätzlich

```
                     , wai >= 3.2.1.1 && < 3.3
                     , wai-middleware-static >= 0.8.1 && < 0.9
                     , warp
                     , websockets
                     , wai-websockets >= 3.0 && < 3.1
                     , time
                     , MonadRandom
                     , parsec
                     , random
```

danach mit

    stack build
	
erstellen


#### Middleware / Statische Inhalte

```haskell
import Prelude hiding (readFile)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)

import Network.Wai (Middleware, Application)
import Network.Wai.Middleware.Static (staticPolicy, addBase)

serveStatic :: Middleware
serveStatic = staticPolicy (addBase "./static")

indexPage :: IO Text
indexPage = readFile "./static/index.html"

```

#### Websocket
Einfacher Beispiel-Websocket, der nur die aktuelle Zeit überträgt

```haskell
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception.Base (AsyncException, handle, fromException, throwIO)
import Control.Monad (forever)

import Network.WebSockets (ServerApp)
import qualified Network.WebSockets as Ws
import qualified Network.Wai.Handler.WebSockets as Wsh

import Data.Time.Clock (getCurrentTime)


websocketLoop :: Ws.Connection -> IO ()
websocketLoop conn = forever $ do
  threadDelay (1000 * 1000)
  time <- T.pack . show <$> getCurrentTime
  Ws.sendTextData conn time
  where 
    ignore e = do
      print e
      case fromException e of
        Just async -> do
          throwIO (async :: AsyncException)
        Nothing    -> return ()

```

#### Spock - App

```haskell
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  cfg <- spockCfg
  app <- mainApp cfg
  Wai.run 8070 app
  

mainApp :: SpockCfg () MySession MyAppState -> IO Application
mainApp cfg = do
  spApp <- spockAsApp $ spock cfg spockApp
  return $ Wsh.websocketsOr Ws.defaultConnectionOptions wsApp spApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
      conn <- Ws.acceptRequest pending_conn
      websocketLoop conn


spockApp :: SpockM () MySession MyAppState ()
spockApp = do

  -- serve static files from local static folder
  middleware serveStatic
  
  get root $ do
    page <- liftIO indexPage
    html page
	
spockCfg :: IO (SpockCfg () MySession MyAppState)
spockCfg = do
  ref <- newIORef 0
  cfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
  pure $
    cfg { spc_sessionCfg =
          (spc_sessionCfg cfg) { sc_cookieName = "CountdownGame" }
        }
```

die Config ändert nur den Namen des Cookies, das Spock für die Session verwendet

#### statische Html

```html
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Countdown-Game</title>
</head>

<body>
<div id="main"></div>
<script src="client.js"></script>
<script>
  var node = document.getElementById('main');
  var loc = window.location;
  var url = "ws://" + loc.host;
  var app = Elm.Main.embed(node, {
     websocketUrl: url
  });
</script>
</body>

</html>
```

#### Erstellen / Ausführen

   stack build
   stack exec Countdown


### Countdown Lib
Source-Files in `src/lib` und `src/web` verschoben, *.cabal* angepasst:

```
library
  exposed-modules:     Countdown.Expressions
                     , Countdown.Parser
                     , Countdown.Game
                     , Countdown.Game.Players
  other-modules:       Countdown.Game.Attempts
                     , Countdown.Game.Random
                     , Countdown.Game.Challanges
  other-modules:       Countdown.Lists
  build-depends:       base >= 4.7 && < 5
                     , aeson
                     , containers
                     , MonadRandom
                     , parsec
                     , random
                     , text
  hs-source-dirs:      src/lib
  default-language:    Haskell2010
                     
                     
executable Countdown
  hs-source-dirs:      src/web
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5
                     , Countdown
                     , Spock >= 0.11
                     , mtl
                     , text
                     , wai >= 3.2.1.1 && < 3.3
                     , wai-middleware-static >= 0.8.1 && < 0.9
                     , warp
                     , websockets
                     , wai-websockets >= 3.0 && < 3.1
                     , time
```

#### Tests
Tests nach `src/test` kopieren

```
test-suite CountdownTests
  type:                exitcode-stdio-1.0
  ghc-options:         -Wall
  hs-source-dirs:      src/test
  main-is:             Spec.hs
  default-language:    Haskell2010
  build-depends:       base >=4.7 && < 5
                     , Countdown
                     , hspec   == 2.*
```


                     


### Spielzustand

```haskell
import qualified Network.HTTP.Types.Status as HttpStatus

data MySession = EmptySession
data MyAppState =
  MyAppState
  { round :: IORef (Maybe Game.RoundState)
  }
  
spockAp = do
  ...
  get "runde" $ do
    state <- getState
    roundOpt <- liftIO $ readIORef (gameRound state)
    case roundOpt of
      Just state -> do
        json state
      Nothing -> do
        setStatus HttpStatus.badRequest400
```

#### JSON über /runde

```
{"herausforderung":{"targetNumber":369,"availableNumbers":[1,2,3,9,15,25]},"versuche":{}}
```

#### Übertragen im Socket

```haskell
```

### Login/Me
Über `/login` (Parameter: `name`) kann sich ein Benutzer einen Namen aussuchen

Über `/me` wird das aktuelle Login (`Player` Struktur) zurückgegeben
