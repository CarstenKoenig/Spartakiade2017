% **Workshop: Haskell**

\newpage

# Linksammlung

## Nützliches
- S. Diehl: [Numeric Tower](http://dev.stephendiehl.com/hask/#numeric-tower)
- J. Bailey: [Haskell CheatSheet](http://blog.codeslower.com/static/CheatSheet.pdf)

## Hintergrund
- S. Diehl: [What I wish I knew when learning Haskell](http://dev.stephendiehl.com/hask/)
- Wikipedia: [Non-strict evaluation](https://en.wikipedia.org/wiki/Evaluation_strategy#Non-strict_evaluation)
- Hudak, Hughes, Jones, Wadler: [A History of Haskell: Being Lazy With Class](http://haskell.cs.yale.edu/wp-content/uploads/2011/02/history.pdf)
- Haskell-Wiki: [Lazy vs. non-strict](https://wiki.haskell.org/Lazy_vs._non-strict)
- H. Apfelmus:  [Incomplete Guide to Lazy Evaluation](https://hackhands.com/guide-lazy-evaluation-haskell/)
- [Overloading in Haskell](http://www.cse.chalmers.se/edu/year/2016/course/TDA452_Functional_Programming/lectures/OverloadingAndTypeClasses.html)

## Tools
- [Stack](https://docs.haskellstack.org/en/stable/README/)
- [Intero](https://haskell-lang.org/intero)
- [VS.Code & Haskelly](https://github.com/haskelly-dev/Haskelly)


\newpage

# Einführung

## Was ist FP

- reine Funktionen erklären
- heute: Typen wirchtig

## Haskell

- **reine** (*pure*) Sprache
- **lazy** Semantik
- **TypeClasses** für Funktionenüberladung

## Tools

- Stack + Emacs + Intero
- Stack + VS.code + Intero + Haskelly

## GHCi

### Beispiel

```haskell
module Main where

main :: IO ()
main = putStrLn "Hello Haskell"
```

GHCi/REPL mit

    stack repl
	
	> :l Demo.hs
    > main
	
etwas ändern

```haskell
main = putStrLn "Hello World"
```

	> :r
	> main
	
### wichtige Kommandos
- `:l Test.hs` load
- `:r` reload
- `:t "Hallo"` type
- `:i Int` info
- `:browse`
- `:h` help
- `:q` quit
- `it`

### Zusätzliche Module
- `:m +Data.Char`
- `import Data.Char (toUpper)

### Erweiterungen
- `:set -XOverloadedStrings`

### Definitionen mit `let`
In GHCi wird jede *Definition* mit `let` durchgeführt (Grund: später)

```haskell
let text = "Hallo"
let zahl = 4
```

falls der Typ benötig wird, kann man diesen bei einer *Expression* mit `::`
anhängen:

```haskell
let zahl = 4 :: Int
```

---

# Werte und Typen

- *Typen* werden in Haskell **groß** geschrieben (`String`, `Int`, ...)
- *Werte/Funktionen/Namen* werden **klein** geschrieben

## Strings
```haskell
let cool = "Haskell"
```

sind tatsächlich *Listen von Char*

```haskell
import Data.Char(toUpper)

map toUpper cool
```

### Text / ByteString
*Prelude* Strings sind nicht allzu performant
Deshalb gibt es noch weitere *Formen* - vor allem `Text` und `ByteString`

#### Beispiel

```haskell
:m +Data.Text
pack "Hallo"
> :t it
```

geht auch einfacher:

```haskell
:set -XOverloadedStrings
"Hallo" :: Text
```

in `.hs` Datei:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module ...
```

## Zahlen

Zahlen fallen in Haskell in sogenannte Typ-Klassen
(siehe auch den Link *Numeric Tower* oben)

die Zahlen-Literale sind ähnlich überladen wie
die Strings mit `OverloadedStrings` gerade.

```haskell
let n = 42
:t n
n `div` 2
:t it
n / 2
:t it
```

gibt auch Ausgefalleneres:

```haskell
import Data.Ratio
5%9 + 7%6
```


## Unit
Ein Typ `()`, der nur einen Wert annehmen kann `() :: ()`

## Bottom
für jeden Typ `t` gilt `undefined :: t` - 
allerdings wirft ein Programm
einen Fehler, wenn `undefined` ausgewertet wird.

Anderes Beispiel:

```haskell
doh :: t
doh = doh
```



# Funktionen

## einfache Definition und Typen

```haskell
plus10 :: Int -> Int
plus10 x = x + 10
```

in GHCi:

```haskell
let plus10 x = x + 10 :: Int
```

### Applikation hat höchste Priorität

```haskell
plus10 5 * 5
```

### Einrückungsregeln

```haskell
plus10 x =
   x + 10
```

### Funktionen als Werte / innere Funktionen

```haskell
plus20 :: Int -> Int
plus20 x =
  let p10 x = x+10
  in p10 x + p10 x

plus15 :: Int -> Int
plus15 x =
  plus10 x + plus5 x
  where plus5 x = x + 5
```

## Lambdas

Werden als `\x -> ...` geschrieben

```haskell
plus10 = \x -> x + 10
```

Konzeptionell gibt (für uns) keinen Unterschied zwischen

```haskell
plus10 x = x+10
```
und
```haskell
plus10 = \x -> x+10
```

Wichtige Funktion

```haskell
id :: x -> x
id = \x -> x
```

## Der Typ
bitte den Typ/Schreibweise `Domain -> Codomain` beachten!

***

### Quiz

Der Typ sagt viel - was kommt für

```Haskell
f :: a -> a
```

überhaupt in Frage?

---

#### Antwort
`id`

*** 




## Mehrere Parameter

```haskell
plus :: Int -> Int -> Int
plus x y = x + y
```

### Partielle Applikation

betrachte Typ von `plus` - den kann man auch als

```haskell
plus :: Int -> (Int -> Int)
```
schreiben!

```haskell
(plus 3) 4 = 7
```

Was ist wohl `plus 3`?

***

#### Vorsicht!

Applikation assoziiert nach links

```haskell
-- falsch:
negate negate 5
```

```haskell
negate (negate 5)
```

**Merksatz:**

- *Typ-Signaturen* sind **recht**-assoziiert,
- *Funktions-Applikation* ist **links**-assoziiert

***

#### QUIZ

Sind folgende Funktionen gleich (was ist ihr Typ)?

```haskell
f a b c = a + b + c
g a = \ b c -> a + b + c
```

Wie kann eine Funktion

```haskell
h :: (Int -> Int) -> Int -> Int
```

aussehen?


##### Antwort
- ja! - der Typ ist `Num a => a -> a -> a -> a`
- `h f x = f x`


***

### Das gilt auch für Typen!

`(->)` ist in der Tat ein **Typ-Konstruktor** (probieren mit `:i` bzw. `:k`)


## Komposition / Verkettung

```haskell
plus25 :: Int -> Int
plus25 = plus 20 . plus 5
```

## Rekursion
Das Standardbeispiel!

```haskell
fact :: Integer -> Integer
fact n = if n <= 1 then 1 else n * fact (n-1)
```

oder (guards)

```haskell
fact2 :: Integer -> Integer
fact2 n
  | n <= 1    = 1
  | otherwise = n * fact2 (n-1)
```

***

### ÜBUNG Fibonnaci Funktion schreiben

Schreibe

```haskell
fib :: Integer -> Integer
```

---

#### Lösung

```haskell
fib n
  | n <= 0    = 0
  | n == 1    = 1
  | otherwise = fib (n-1) + fib (n-2) 
```




# Daten
## einfache Definition

```haskell
data Boolean = Wahr | Falsch
```

versuche `Wahr` auszuwerten

**fix:**
```haskell
data Boolean = Wahr | Falsch
  deriving Show
```

### einfache Funktion

```haskell
nicht :: Boolean -> Boolean
nicht Wahr = Falsch
nicht Falsch = Wahr
```

***

#### Übung:

Schreibe: `und` und `oder`

##### Lösung
```haskell
und :: Boolean -> Boolean -> Boolean
und Wahr x = x
und Falsch _ = Falsch

oder :: Boolean -> Boolean -> Boolean
oder Wahr _ = Wahr
oder Falsch x = x
```

***

#### Bemerkung
*lazyness* for the win:

```haskell
und Falsch undefined
oder Wahr undefined

und Wahr undefined
```


## mit anderen Daten

```haskell
data Benutzer
  = Unbekannt
  | Name String
  deriving Show

begruessung :: Benutzer -> String
begruessung Unbekannt = "Hallo Fremder"
begruessung (Name x)  = "Hallo " ++ x
```

Haskells/FP Antwort auf `null`:

```haskell
data Option a
  = None
  | Some a

  
withDefault :: a -> Option a -> a
withDefault a None = a
withDefault _ (Some a) = a
```

#### Übung:

Schreibe: `mmap :: (a -> b) -> Option a -> Option b`

##### Lösung
```haskell
mmap :: (a -> b) -> Option a -> Option b
mmap f None = None
mmap f (Some a) = Some (f a)
```

***

damit wird `Option` zu einem Funktor:

```haskell
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
```

mit:

```haskell
instance Functor Option where
  fmap = mmap
``` 

den Typ gibt es schon unter dem Namen `Maybe`


## rekursive Definition

```haskell
data Nat = Null | Succ Nat
  deriving Show

eins :: Nat
eins = Succ Null

zwei :: Nat
zwei = Succ eins
```

### Funktionen
```haskell
zuInteger :: Nat -> Integer
zuInteger Null     = 0
zuInteger (Succ n) = 1 + zuInteger n
```

***

#### Übung
schreibe eine Funktion
(ignoriere negative Zahlen)

```haskell
zuNat :: Integer -> Nat
```

---

##### Lösung
```haskell
zuNat :: Integer -> Nat
zuNat 0 = Null
zuNat n = Succ (zuNat (n-1))
```

***


Addition:

```haskell
plus :: Nat -> Nat -> Nat
plus Null m = m
plus (Succ n) m = Succ (plus n m)
``` 

#### Übung
schreibe eine Funktion

```haskell
mult :: Nat -> Nat -> Nat
```

Testen mit:

```haskell
zuInteger (mult (zuNat 5) (zuNat 6)) -- = 30
```

---

##### Lösung
```haskell
mult :: Nat -> Nat -> Nat
mult Null _ = Null
mult (Succ n) m = plus m (mult n m)
```

***






## Tupel

```haskell
data Paar a b = Paar a b
  deriving Show

erstes :: Paar a b -> a
erstes (Paar a _) = a

zweites :: Paar a b -> b
zweites (Paar _ b) = b
``` 

***

### Übung
Schreibe Funktionen:

```haskell
erstes :: Paar a b -> a

zweites :: Paar a b -> b
```

#### Lösung
```haskell
erstes :: Paar a b -> a
erstes (Paar a _) = a

zweites :: Paar a b -> b
zweites (Paar _ b) = b
```

***

gibt es alles schon in Haskell:

```haskell
type Paar a b = (a,b)

erstes = fst
zweites = snd
``` 

### Pattern-Matching
funktioniert nicht nur in Funktionen:

```haskell
carsten = ("Carsten", 37)

(name, alter) = carsten

(name, _) = carsten

(name, 37) = carsten

(name, 44) = carsten -- Fehler
```

***

### Übung

- 1. gebe ein Beispiel für ein Tupel vom Typ
   * `(Bool, (), Char)`
   * `(String, (Int, Int), Bool)`
- 2. Schreibe Funktionen
   * `curryIt :: ((a,b) -> c) -> (a -> b -> c)`
   * `uncurryIt :: (a -> b -> c) -> ((a,b) -> c)`
   
#### Lösung

```haskell
curryIt :: ((a,b) -> c) -> (a -> b -> c)
curryIt f a b = f (a,b)

uncurryIt :: (a -> b -> c) -> ((a,b) -> c)
uncurryIt f (a,b) = f a b
```

***


## Listen


```haskell
data Liste a
  = Leer
  | Cons a (Liste a)
  deriving Show
```

Haskell kennt Listen aber schon primitv:

```haskell
data [] a
  = []
  | a : [a]
  deriving Show
```

### Beispiele:

```haskell
l = 1 : (2 : (3 : (4 : (5 : []))))
l = [1, 2, 3, 4, 5]
l = [1..5]
```

### Head/Tail/Cons

```haskell
head [1..5] -- = 1
tail [1..5] -- = [2,3,4,5]

[1..5] ++ [6..10] -- = [1..10]
```

***

### Übung

schreibe Dein eigenes/rekursives `(++)` (concat):

```haskell
concat' :: [a] -> [a] -> [a]
```

#### Lösung:

```haskell
concat' :: [a] -> [a] -> [a]
concat' [] ys     = ys
concat' (x:xs) ys = x : concat' xs ys
```

***


## größere Übung

Lasse die Teilnehmer das CoinChange implementieren:

Gegeben Münzen `[200,100,50,20,10,5,2,1]`

- Für jeden Eingabebetrag `betrag` (in Cent)
- Gebe eine *minimale* Liste von Münzen aus, die den Betrag ergeben

**Beispiel:**

```haskell
coinChange 347 = [200,100,20,20,5,2]
```

### Lösung

```haskell
type Münze = Int
type Betrag = Int

münzen :: [Münze]
münzen = [200,100,50,20,10,5,2,1]

coinChange :: Betrag -> [Münze]
coinChange = coinChange' münzen
  where
    coinChange' [] _ = []
    coinChange' (m:ms) b
      | m <= b = m : coinChange' (m:ms) (b-m)
      | otherwise = coinChange' ms b
```

***




# List Comprehensions und Funktionen

```haskell
quadratZahlen :: [Integer]
quadratZahlen = [ n*n | n <- [1..] ]

geradeQuadratZahlen :: [Integer]
geradeQuadratZahlen = [ n | n <- quadratZahlen, even n ]
```

am besten mit `take`:

```haskell
take 5 quadratZahlen
```

## Kombinatorischen Spass

```haskell
farbenUndZahlen :: [(String,Int)]
farbenUndZahlen = [ (f,z) | f <- farben, z <- zahlen ]
```

## Standardfunktionen

ähnlich: `drop`, sehr nützlich: `dropWhile` und `takeWhile`

Probieren (Typ anschauen, ausprobieren)

- `sum`
- `product`
- `length`
- `null`
- `map`
- `filter`
- `Data.List.find`
- `Data.List.sort`
- `Data.List.group`
- `any`
- `all`
- `foldl`
- `concatMap`

## Beispiel: Projekt Euler - Problem 7
[Link](https://projecteuler.net/problem=7)

> By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
>
> What is the 10 001st prime number?

```haskell
solution :: Integer
solution = primes !! 10000


isPrime :: (Integral a) => a -> Bool
isPrime p = not $ any (`divides` p) $ takeWhile (\n -> n*n <= p) [2..p]
  where a `divides` b = b `mod` a == 0


primes :: (Integral a) => [a]
primes = [ n | n <- [2..], isPrime n ]
```

## Beispiel : Projekt Euler - Problem 45
[Link](https://projecteuler.net/problem=45)

> Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:
> ```
> Triangle:
>   n(n+1)/2
>   1, 3, 6, 10, 15, ...
> Pentagonal:
>   n(3n-1)/2
>   1, 5, 12, 22, 35, ...
> Hexagonal:
>   n(2n-1)
>   1, 6, 15, 28, 45, ...
>```
> It can be verified that T285 = P165 = H143 = 40755.
>
> Find the next triangle number that is also pentagonal and hexagonal.

```haskell
euler45 :: Integer
euler45 = head (dropWhile (<= 40755) loesungen)

loesungen :: [Integer]
loesungen = schneide tris (schneide pents hexs)

tris :: [Integer]
tris = [ n*(n+1) `div` 2 | n <- [1..] ]

pents :: [Integer]
pents = [ n*(3*n-1) `div` 2 | n <- [1..] ]

hexs :: [Integer]
hexs = [ n*(2*n-1) | n <- [1..] ]

schneide :: Ord a => [a] -> [a] -> [a]
schneide [] _ = []
schneide _ [] = []
schneide (x:xs) (y:ys)
  | x == y = x : schneide xs ys
  | x < y = schneide xs (y:ys)
  | x > y = schneide (x:xs) ys
```


# IO / Monaden
Verbindung mit der *richtigen Welt* funktioniert für
*reine* Funktionen nicht (Beispiel Uhrzeit)

Die Lösung in Haskell ist der `IO` Type

Du kannst `IO a` lesen als: eine Aktion, die
ein `a` produziert.

Solche Aktionen oder *Commandos* können beliebig
in Haskell definiert werden, **ausgeführt** werden
Sie aber nur, wenn sie in *GHCi* ausgewertet werden.

Wenn Du ein Programm kompilierst, wird die spezielle
`main :: IO ()` Aktion als *Einsprungspunkt* benutzt,
d.h. das Programm fängt damit an, dieses `main`
Kommando auszuführen, wenn Du es startest.

Kommandos, die nur Seiteneffekte enhalten sind meist
vom Typ `IO ()`

## Beispiele:

- `putStrLn` und `print` schreiben Ausgaben nach **stdout**
- `getLine` liest eine Zeile (bis zum `\n`) von **stdin**

## Verknüpfen / `do`-Notation
Kommandos können in einem speziellen `do`-Block miteinander
verknüpft werden - das ist dann im Prinzip *imperative Programmierung*
- die Kommandos werden Zeile für Zeile ausgeführt:

```haskell
main :: IO ()
main = do
  putStr "Wie ist Dein Vorname? "
  vorname <- getLine
  putStr "Und Dein Nachname? "
  nachname <- getLine
  let name = vorname ++ " " ++ nachname
  putStrLn ("Hallo " ++ name)
```

- mit `x <- berechnung` wird die `berechnung :: IO a` ausgeführt, danach
enthält `x` das Ergebnis der Berechnung (vom Typ `a`)
- mit `let x = wert` ist *reine* Zuweisung (wie wir sie bisher kennen) damit
ist auch klar, warum in *GHCi* mit `let` gearbeitet werden muss - wir befinden
uns dort in so einer Art *interaktiven* `IO` Umgebung.
- `return x :: IO x` ist eine Berechnung, die `x` zurückgibt (ohne Seiteneffekte)

## Beispiel Uhr
```haskell
import Control.Monad (forever)
import Control.Concurrent (ThreadId)
import qualified Control.Concurrent as CC
import qualified Data.Time as DT

starteUhr :: IO (() -> IO ())
starteUhr = do
  thId <- uhr
  return $ \ () -> CC.killThread thId
  

uhr :: IO ThreadId
uhr = CC.forkIO $ forever $ do
  now <- DT.getCurrentTime
  print now
  CC.threadDelay 1000000
```

```haskell
> stop <- starteUhr
2017-03-15 15:29:09.392439 UTC
2017-03-15 15:29:10.393733 UTC
2017-03-15 15:29:11.395641 UTC
2017-03-15 15:29:12.397552 UTC
2017-03-15 15:29:13.399476 UTC
stop()
```

## `do` Notation funktioniert auch für andere Monaden

### Maybe
```haskell
halfIfEven :: Int -> Maybe Int
halfIfEven n
  | even n    = Just (n `div` 2)
  | otherwise = Nothing

minus1IfOdd :: Int -> Maybe Int
minus1IfOdd n
  | odd n     = Just (n-1)
  | otherwise = Nothing

nowBoth :: Int -> Maybe Int
nowBoth n = do
  n' <- halfIfEven n
  minus1IfOdd n'
```

In GHCi:

```haskell
> nowBoth 6
Just 2
> nowBoth 8
Nothing
```

### Listen
```haskell
kombinationen :: [(Int,String)]
kombinationen = do
  z <- [1..3]
  s <- ["A","B","C"]
  return (z,s)
```

In GHCi:

```haskell
> kombinationen
[(1,"A"),(1,"B"),(1,"C"),(2,"A"),(2,"B"),(2,"C"),(3,"A"),(3,"B"),(3,"C")]
```

### some Fun
```haskell
import Control.Monad (replicateM)

replicateM :: Applicative m => Int -> m a -> m [a] -- denke: Monad m => Int -> m a -> m [a]
```

was könnte

```haskell
guess = replicateM 2 (Just 3)
```

sein?

---

Was ist mit

```haskell
guess = replicateM 3 "ABC"
```

