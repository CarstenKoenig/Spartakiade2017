# Workshop: ELM

# Linksammlung

## Elm
- [Homepage](http://elm-lang.org/)
- [Packages Docs](http://package.elm-lang.org/)
- [Syntax](http://elm-lang.org/docs/syntax)
- [Javascript -> Elm](http://elm-lang.org/docs/from-javascript)
- [Elm Bootstrap](http://elm-bootstrap.info/)

## nützliche Docs
- [Elm Core](http://package.elm-lang.org/packages/elm-lang/core/latest/)
- [Elm Html](http://package.elm-lang.org/packages/elm-lang/html/latest/)
- [Elm Bootstrap Docs](http://package.elm-lang.org/packages/rundis/elm-bootstrap/latest)

## Hintergrund
- [Elm Records](http://elm-lang.org/docs/records)
- [the Elm Architecture](https://guide.elm-lang.org/architecture/)

## Tools
- [Installation](https://guide.elm-lang.org/install.html)
- [elm-format](https://github.com/avh4/elm-format#installation)
- [elm-oracle](https://github.com/ElmCast/elm-oracle)
- [vscode-elm](https://github.com/sbrink/vscode-elm)

\newpage

# Installation
## Elm
Elm selbst kann unter:

- **Windows** entweder als [Installer](http://install.elm-lang.org/Elm-Platform-0.18.exe) oder über [NPM](https://www.npmjs.com/package/elm) installiert werden.
- **Mac** ebenfalls als [Installer](http://install.elm-lang.org/Elm-Platform-0.18.pkg) oder [NPM](https://www.npmjs.com/package/elm)
- **Linux** über [NPM](https://www.npmjs.com/package/elm)

installiert werden.

Persönlich bevorzuge ich den Weg über **NPM** weil das überall geht:

```
npm install -g elm
```

bzw.

```
sudo install -g elm
```

## Elm Format
*Elm Format* ist sehr nützlich (automatische Quellcodeformatierung) und ich empfehle es mit zu installieren, auch wenn es am Anfang etwas
gewöhnungsbedürftig ist.

*Elm Format* kann man am einfachsten installieren, indem man die Binary irgendwo in seinen Pfad ablegt.

Die Binaries findet man für:

- **Windows** [hier](http://install.elm-lang.org/Elm-Platform-0.18.pkg)
- **Mac** [hier](https://github.com/avh4/elm-format/releases/download/0.5.2-alpha/elm-format-0.18-0.5.2-alpha-mac-x64.tgz)
- **Linux** [hier](https://github.com/avh4/elm-format/releases/download/0.5.2-alpha/elm-format-0.18-0.5.2-alpha-linux-x64.tgz)

## Elm Oracle
Manche Editor-Plugins (*elm-vim*, *atom-elm*, *elm-mode* (Emacs), *Elm.tmLanguage* (Sublime), *elm-light* (LightTable)) benötigen *Elm Oracle*

Die Installation läuft über **NPM**:

```
npm install -g elm-oracle
```

bzw.

```
sudo npm install -g elm-oracle
```

## Editorsupport
### VS.code
siehe [vscode-elm](https://marketplace.visualstudio.com/items?itemName=sbrink.elm)
für *elm-format* kann noch [VS.code elm-format](https://marketplace.visualstudio.com/items?itemName=abadi199.elm-format) installiert werden

### Emacs
ist in [**MELPA** (`elm-mode`)](https://github.com/jcollard/elm-mode) verfügbar, *elm-oracle* und *elm-format* sollten installiert sein.

hier ist der relevante Teil meiner Emacs-Config:

```commonlisp
;; -----------------------------------------------------------------------------
;; ELM
(require 'elm-mode)
(setq elm-format-on-save t)
(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
(add-hook 'elm-mode-hook 'linum-mode)
(add-hook 'elm-mode-hook 'flycheck-mode)

;; Company Backend
(add-to-list 'company-backends 'company-elm)

;; Flycheck
(require 'flycheck-elm)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))
``` 

außerdem habe ich folgendes in `customize-mode` (bzw. `.gnu-emacs-custom`), damit `elm-format` beim Speichern benutzt wird:

```commonlisp
 '(elm-format-on-save t)
```


## andere
siehe "Configure Your Editor" [hier](https://guide.elm-lang.org/install.html)




# Einführung

## Start
Einfach in ein Verzeichnis und dann

    elm make
    
damit wird eine `elm.package.json` mit ein paar
Standard-Modulen angelegt.

Jetzt kann eine (z.B. `main.elm`) Elm Datei angelegt werden.

Die Datei sieht in der Regel ungefähr so aus:

```idris
module Demo exposing (..)

import Html


main =
    Html.text "Hallo Elm"
```

der Name des Moduls sollte dem der Datei entsprechen (darüber findet
Elm die Module im Dateisystem)

Wird ein `main` angegeben, dann muss es eines der Typen:

- `Html` (`import Html` nicht vergessen): HTML Dokument
- `Svg` ein Bild
- `Program` (das ist die Regel) für eine Applikation

Der `main` Wert wird benötigt, wenn das Modul der Einstiegspunkt
für eine Applikation ist oder wenn man den *elm reactor* verwenden
möchte (sonst zeigt dieser nur eine leere Seite).

Diese wird kompiliert mit

- `elm make main.elm` erzeugt eine einfache `index.html` mit der gesamten Anwendung
- in der Regel aber nützlicher: `elm make main.elm --output datei.js`

Einbetten kann man das dann in Html mit:

```html
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>Elm eingebettet</title>
</head>

<body>
<div id="main"></div>
<script src="client.js"></script>
<script>
  var node = document.getElementById('main');
  var app = Elm.Main.embed(node);
</script>
</body>

</html>
```

Dabei ist natürlich die `src` etc. zu ersetzen.



## die Repl
über 

    elm repl
    
jetzt kann man mit `import Demo` das Modul importieren und *benutzen*

Einmal importiert, reicht es übrigens, die Datei zu speichern, beim auswerten
einer Expression wird die Datei in der *REPL* neu geladen.


## der Reactor

aufruf mit

    elm repl
    
danach Seite [localhost:8000](http://localhost:8000)

von dort kann man Elm-Dateien automatisch kompilieren/anzeigen lassen.






# Taschenrechner

    mkdir Calc
    cd Calc
    elm make
    touch Calc.elm
    elm reactor
    

Starten mit:

```idris
module Calc exposing (..)

import Html exposing (Html)


main : Html msg
main =
    Html.text "Hi"
```

`Html msg` heißt: die Rückgabe ist eine `Html` Darstellung
deren Aktionen (später) eine Nachricht vom *generischen* Typ
`msg` zurückgebenkönnen (für den Moment bitte ignorieren)

## Das Display
sagen wir ein `<h1>` Element:

```idris
main : Html msg
main =
    display


display : Html msg
display =
    Html.h1 [] [ Html.text "0" ]
```

Ein paar Styles:

```idris
import Html.Attributes as Attr

display =
    Html.h1
        [ Attr.style
            [ ( "background", "lightgray" )
            , ( "color", "black" )
            , ( "text-align", "right" )
            ]
        ]
        [ Html.text "0" ]
```

Zahl ausgeben:

```idris
display : Int -> Html msg
display zahl =
    Html.h1
        [ Attr.style
            [ ( "background", "lightgray" )
            , ( "color", "black" )
            , ( "text-align", "right" )
            ]
        ]
        [ Html.text (toString zahl) ]
```

## Zahlenblock

ein einzelner Knopf:

```idris
knopf : String -> Html msg
knopf text =
    Html.button
        [ Attr.style
            [ ( "width", "30px" )
            , ( "height", "30px" )
            ]
        ]
        [ Html.text text ]
        
main =
    Html.div []
        [ display 42
        , knopf "1"
        ]
```

eine Reihe mit Knöpfen:

```idris
knopfReihe : List String -> Html msg
knopfReihe texte =
    Html.div
        []
        (List.map knopf texte)

main =
    Html.div []
        [ display 42
        , knopfReihe [ "7", "8", "9", "*" ]
        ]
```

`String` ist nicht schön wir sollten den Typ umbennenen:

```idris
type alias Knopf = String
```

der ganze Tastaturblock (Übung)

```idris
tastatur : List (List Knopf) -> Html msg
tastatur knopfReihen =
    Html.div
        []
        (List.map knopfReihe knopfReihen)


main =
    Html.div [ Attr.style [ ( "width", "120px" ) ] ]
        [ display 42
        , tastatur
            [ [ "7", "8", "9", "*" ]
            , [ "4", "5", "6", "/" ]
            , [ "1", "2", "3", "+" ]
            , [ "0", ",", "=", "-" ]
            ]
        ]
```

## Elm Architektur

### anlegen

Übergang zu `Program`

```idris
main : Program Never () msg
main =
    Html.beginnerProgram
        { model = ()
        , view = view
        , update = update
        }


update : msg -> model -> model
update msg model =
    model


view : () -> Html msg
view model =
    Html.div [ Attr.style [ ( "width", "120px" ) ] ]
        [ display 42
        , tastatur
            [ [ "7", "8", "9", "*" ]
            , [ "4", "5", "6", "/" ]
            , [ "1", "2", "3", "+" ]
            , [ "0", ",", "=", "-" ]
            ]
        ]
```

- Das `model` ist der Zustand des Programs
- die `view` stellt den Zustand als `Html` dar
- `update` benutzt Nachrichten `msg` um aus einem
`model` ein neus zu erzeugen - das ist der einzige Platz im Programm an dem sich
der Zustand ändern kann!

### das Model

Der Zustand bzw. das Model soll zunächst die angezeigte Zahl sein:

```idris
type alias Model = Integer

main : Program Never Model msg
main =
    Html.beginnerProgram
        { model = 0


update : msg -> Model -> Model


view : Model -> Html msg
view model =
    Html.div [ Attr.style [ ( "width", "120px" ) ] ]
        [ display model

```

### die Nachrichten

Bei Knopfdruck sollen nun Nachrichten erzeugt werden,
dafür brauchen wir erst einmal einen beschreibenden
Typ:

```idris
type Message
    = ZifferGedrueck Int
    | OperatorGedrueck Operator


type Operator
    = Plus
    | Minus
    | Mal
    | Geteilt
    | Istgleich
    | Komma
    
   
main : Program Never Model Message
   
update : Message -> Model -> Model

view : Model -> Html Message

```

### Events
damit ein Knopf eine Nachricht auslösen kann, müssen wir
ihm das `onClick` Attribut setzen, das aber einen
Nachrichten-Wert benötigt, d.h. wir sollten unseren
`Knopf` Typ erweitern bzw. in einen Record umwandeln:

```idris
type alias Knopf =
    { text : String
    , message : Message
    }
```

das hat ein paar Fehler zur Folge, die wir beheben:

```idris
knopf knopf =
    ...
        [ Html.text knopf.text ]
    
```

der Ziffernblock ist lästig, deswegen führen wir eine lokale Hilfsfunktion ein:

```idris
view : Model -> Html Message
view model =
    let
        zif x =
            Knopf (toString x) (ZifferGedrueck x)

        op s o =
            Knopf s (OperatorGedrueck o)

        opPl =
            op "+" Plus

        opMi =
            op "-" Minus

        opMa =
            op "*" Mal

        opDu =
            op "/" Geteilt

        opGl =
            op "=" Istgleich

        opKo =
            op "," Komma
    in
        Html.div [ Attr.style [ ( "width", "120px" ) ] ]
            [ display model
            , tastatur
                [ [ zif 7, zif 8, zif 9, opMa ]
                , [ zif 4, zif 5, zif 6, opDu ]
                , [ zif 1, zif 2, zif 3, opPl ]
                , [ zif 0, opKo, opGl, opMi ]
                ]
            ]
```

jetzt können wir das Event setzen:

```idris
import Html.Events as Ev


knopf : Knopf -> Html Message
knopf knopf =
    Html.button
        [ Attr.style
            [ ( "width", "30px" )
            , ( "height", "30px" )
            ]
        , Ev.onClick knopf.message
        ]
        [ Html.text knopf.text ]
```

jetzt müssen wir auch überall `msg` mit `Message` tauschen,
weil die Nachricht jetzt überall konkret spezifiziert wurde.

> Aber es tut sich immer noch nichts...!

Im Moment ist die `update` Funktion ja auch noch richtig dumm!

Setzen wir wenigstens die Ziffern:

```idris
update : Message -> Model -> Model
update msg model =
    case msg of
        ZifferGedrueck z ->
            model * 10 + z

        _ ->
            model
```

### Operationen ... funktional Denken
Ok wie machen wir das mit den Operatoren?

Offensichtlich müssen wir uns die letzte Zahl
sowie die Operation mit merken, d.h. wir müssen
das Model auf jeden Fall erweitern.

Warum aber beides merken? Wir benutzen einfach
eine Funktion `Int -> Int`!

```idris
type alias Model =
    { anstehendeOperation : Int -> Int
    , aktuelleZahl : Int
    }
```

natürlich sind jetzt erst einmal die Syntaxfehler
zu korrigieren:

```idris
main =
    Html.beginnerProgram
        { model = Model identity 0
        , view = view
        , update = update
        }
        
update msg model =
    case msg of
        ZifferGedrueck z ->
            { model | aktuelleZahl = model.aktuelleZahl * 10 + z }
        
```

***

#### Übung
Die anderen Fehler korrigieren

```idris
display : Model -> Html msg
display model =
    ...
        [ Html.text (toString model.aktuelleZahl) ]
```

***

das können wir jetzt im Update nutzen:

```idris
        OperatorGedrueck op ->
            case op of
                Istgleich ->
                    let
                        neueZahl =
                            model.anstehendeOperation model.aktuelleZahl

                        neueOperation =
                            identity
                    in
                        { model | aktuelleZahl = neueZahl }

                Plus ->
                    let
                        neueZahl =
                            0

                        neueOperation =
                            \operand2 -> model.aktuelleZahl + operand2
                    in
                        { model
                            | aktuelleZahl = neueZahl
                            , anstehendeOperation = neueOperation
                        }
                _ ->
                    model
```

da steckt ziemlich viel wiederholung drin, machen wir das kürzer:

```idris
        OperatorGedrueck op ->
            let
                ( neueZahl, neueOperation ) =
                    case op of
                        Istgleich ->
                            ( model.anstehendeOperation model.aktuelleZahl
                            , identity
                            )

                        Plus ->
                            ( 0
                            , \operand2 -> model.aktuelleZahl + operand2
                            )

                        _ ->
                            ( model.aktuelleZahl, model.anstehendeOperation )
            in
                { model
                    | aktuelleZahl = neueZahl
                    , anstehendeOperation = neueOperation
                }

```

Sieht jemand den Fehler?

> Vorsicht:
> Test `1+2+3`

```idris
\operand2 -> model.anstehendeOperation model.aktuelleZahl + operand2
```

***

#### Übung
andere Operatoren implementieren (Komma ignorieren)

```idris
Istgleich ->
    ( model.anstehendeOperation model.aktuelleZahl
    , identity
    )

Plus ->
    ( 0
    , \operand2 -> model.anstehendeOperation model.aktuelleZahl + operand2
    )

Minus ->
    ( 0
    , \operand2 -> model.anstehendeOperation model.aktuelleZahl - operand2
    )

Mal ->
    ( 0
    , \op
```

****

Problem bei `/` gemerkt? Wir müssen auf `Float` ausweichen!

```idris
type alias Model =
    { anstehendeOperation : Float -> Float
    , aktuelleZahl : Float
    }


type Message
    = ZifferGedrueck Float
    | OperatorGedrueck Operator


...

Geteilt ->
    ( 0
    , \operand2 -> model.anstehendeOperation model.aktuelleZahl / operand2
    )

```

***

#### Übung
Überlegen wie das Komma funktioniert

```idris
type alias Model =
    { anstehendeOperation : Float -> Float
    , aktuelleZahl : Float
    , stelle : Float
    }

...

update : Message -> Model -> Model
update msg model =
    case msg of
        ZifferGedrueck z ->
            if model.stelle == 0 then
                { model | aktuelleZahl = model.aktuelleZahl * 10 + z }
            else
                { model
                    | aktuelleZahl = model.aktuelleZahl + z * 10 ^ (-1 * model.stelle)
                    , stelle = model.stelle + 1
                }

        OperatorGedrueck op ->
            let
                ( neueZahl, neueOperation, neueStelle ) =
                    case op of
                        Istgleich ->
                            ( model.anstehendeOperation model.aktuelleZahl
                            , identity
                            , 0
                            )

                        Plus ->
                            ( 0
                            , \operand2 -> model.anstehendeOperation model.aktuelleZahl + operand2
                            , 0
                            )

                        Minus ->
                            ( 0
                            , \operand2 -> model.anstehendeOperation model.aktuelleZahl - operand2
                            , 0
                            )

                        Mal ->
                            ( 0
                            , \operand2 -> model.anstehendeOperation model.aktuelleZahl * operand2
                            , 0
                            )

                        Geteilt ->
                            ( 0
                            , \operand2 -> model.anstehendeOperation model.aktuelleZahl / operand2
                            , 0
                            )

                        Komma ->
                            ( model.aktuelleZahl
                            , model.anstehendeOperation
                            , if model.stelle == 0 then
                                1
                              else
                                model.stelle
                            )
            in
                { model
                    | aktuelleZahl = neueZahl
                    , anstehendeOperation = neueOperation
                    , stelle = neueStelle
                }
```

***








# Uhr (Sub, Cmd, Task, Result)

    mkdir Uhr
    cd Uhr
    elm make
    touch Uhr.elm
    elm reactor
    

Starten mit:

```idris
module Uhr exposing (..)

import Html exposing (Html)
import Time exposing (Time)


type alias Model =
    Time


type Message
    = Tick Time


main : Program Never Time Message
main =
    Html.program
        { init = ( 0, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    model ! []


view : Model -> Html Message
view time =
    Html.text (toString time)
```

Jetzt hat kann Update zusätzlich ein Kommando auslösen (im Prinzip 
Seiteneffekte). Ein Kommando führt etwas - meist im Hintergrund - aus und
liefert löst dann eine weitere Message aus.

Außerdem können jetzt noch Model abhängige Subscriptions eingerichtet werden.

Wir begnügen uns hier mit einem einfachen Zeitsignal jede Sekunde:

```idris
main = 
    Html.program
       ...
        , subscriptions = always jedeSekunde

update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        Tick zeit ->
            zeit ! []


jedeSekunde : Sub Message
jedeSekunde =
    Time.every Time.second Tick

```

das zeigt aber nur eine lange Zahl an (die internen Ticks).



Glücklicherweise gibt es im [elm-community/elm-time](http://package.elm-lang.org/packages/elm-community/elm-time/latest) 
entsprechende Hilfsfunktionen:

    elm package install elm-community/elm-time

```idris
import Time.DateTime as DT exposing (DateTime)

type alias Model =
    DateTime

type Message
    = Tick Time

main : Program Never DateTime Message
main =
    Html.program
        { init = ( DT.dateTime DT.zero, Cmd.none )
    ...
    
    
update msg model =
    case msg of
        Tick zeit ->
            DT.fromTimestamp zeit ! []


view time =
    Html.text (zeigeZeit time)


zeigeZeit : DateTime -> String
zeigeZeit zeit =
    let
        zweiStellig getter =
            let
                zeitString =
                    toString (getter zeit)
            in
                case String.length zeitString of
                    0 ->
                        "00"

                    1 ->
                        "0" ++ zeitString

                    _ ->
                        zeitString
    in
        zweiStellig DT.hour
            ++ ":"
            ++ zweiStellig DT.minute
            ++ ":"
            ++ zweiStellig DT.second

```

Es wäre doch noch nett, wenn wir am Anfang nicht eine Sekunde `00:00:00` sehen
würden.

Dafür können wir den [now Task](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Time)
benutzen:

```idris
import Task

type Message
    = Tick Time
    | NoOp


main : Program Never DateTime Message
main =
    Html.program
        { init = ( DT.dateTime DT.zero, systemZeit )
        ...


update ...
    case msg of
        NoOp -> model ! []


systemZeit : Cmd Message
systemZeit =
    let
        onResult res =
            case res of
                Result.Err fehler ->
                    Debug.log (toString fehler) NoOp

                Result.Ok zeit ->
                    Tick zeit
    in
        Time.now
            |> Task.attempt onResult


```

Jetzt wird im Init das `systemZeit` Kommando verwendet,
dieses erstellt einen Task um die Systemzeit zu erhalten
und verwendet `Task.attempt` um die eine zurückgegebene
[`Result`-Struktur](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Result#Result) 
in eine Nachricht umzuwandeln.

`Result` hat dabei zwei mögliche Werte: `Result.Err` mit einem Fehler oder 
`Result.Ok` mit dem Ergebnis.

Hier wird im Fehlerfall einfach auf die Konsole gelogt (`Debug.log`) und sonst
das entsprechende Ereignis zurückgegeben.

Um den Fehler nicht behandeln zu müssen wird ein `NoOp` Fall in die Nachrichten
eingefügt, der, nomen est omen, das Model unangetastet lässt.



# Kommunikationmit dem Server (AJAX/JSON Decoder)

## Beispiel Json
```
{"runde":{"herausforderung":{"targetNumber":100,"availableNumbers":[2,3,4,5,6]}
,"versuche":[{"wert":90,"punkte":5,"spieler":{"nickName":"Spieler1"}}
,{"wert":null,"punkte":0,"spieler":{"nickName":"Spiele2r"}}]}
,"naechsteRundeUm":"2017-03-16T16:52:34.932921Z"}
```

```idris
import Json.Decode as Json exposing (Decoder)
import Http as Http exposing (Request, Error(..))
import Time.ZonedDateTime as Time exposing (ZonedDateTime)
import Time.TimeZones exposing (europe_berlin)


type alias SpielZustand =
    { runde : Maybe SpielRunde
    , endetUm : ZonedDateTime
    }


type alias SpielRunde =
    { herausforderung : Herausforderung
    , versuche : List Versuch
    }


type alias Herausforderung =
    { zielZahl : Int
    , zahlenvorat : List Int
    }


type alias Versuch =
    { wert : Maybe Int
    , punkte : Int
    , spieler : Spieler
    }


type alias Spieler =
    { nick : String
    }


getExample : (Error -> msg) -> (SpielZustand -> msg) -> Cmd msg
getExample =
    getCmd "/example" decodeZustand
    
    
decodeZustand : Decoder SpielZustand
decodeZustand =
    Json.map2 SpielZustand
        (Json.at [ "runde" ] (Json.nullable decodeRunde))
        (Json.at [ "naechsteRundeUm" ] decodeUtc)


decodeUtc : Decoder ZonedDateTime
decodeUtc =
    let
        parseUtc inp =
            Time.fromISO8601 (europe_berlin ()) inp
                |> Result.withDefault (Time.zonedDateTime (europe_berlin ()) Time.zero)
    in
        Json.map parseUtc Json.string



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
    Json.list decodeVersuch


decodeVersuch : Decoder Versuch
decodeVersuch =
    let
        decodeWert =
            Json.at [ "wert" ] (Json.maybe Json.int)

        decodePunkte =
            Json.at [ "punkte" ] Json.int

        decodeSp =
            Json.at [ "spieler" ] decodeSpieler
    in
        Json.map3 Versuch decodeWert decodePunkte decodeSp



decodeSpieler : Decoder Spieler
decodeSpieler =
    let
        decodeNick =
            Json.at [ "nickName" ] Json.string
    in
        Json.map Spieler decodeNick
```
