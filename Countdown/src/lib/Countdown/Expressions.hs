{-# LANGUAGE OverloadedStrings #-}

module Countdown.Expressions
       ( Operand (..)
       , Expression (..)
       , isValidOp
       , apply
       , values
       , eval
       , solutions
       , solutions'
       )where

import Countdown.Lists (subbags, notEmptySplit)

-- | die Verfügbaren Operationen: Addition, Subtraktion,
--   Multiplikation und Division
data Operand
  = Add | Sub | Mul | Div
  deriving (Eq, Ord)

-- | prüft ob eine Operation nach den Spielregeln ok ist
-- nicht ok sind:
--
--  * nicht ganzzahlige Divisionen
--  * Subtraktionen, die Zahlen kleiner-gleich 0 ergeben
--
-- >>> isValidOp Div 4 0
-- False
--
-- >>> isValidOp Mul 5 10
-- True
isValidOp :: (Ord a, Integral a) => Operand -> a -> a -> Bool
isValidOp Add _ _ = True
isValidOp Sub x y = x > y
isValidOp Mul _ _ = True
isValidOp Div x y = x `mod` y == 0

-- | führt eine Operation durch, die durch einen `Operand` und
-- die beiden zugehörigen Argumente gegeben ist.
-- Dabei wird nicht auf die Spielregeln geachtet!
--
-- >>> apply Add 4 5
-- 9
--
-- >>> apply Div 8 4
-- 2
apply :: Integral a => Operand -> a -> a -> a
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = div

-- | Darstellung eines Rechenausdrucks für das Spiel
--
-- Beispiel 3+(4*5)
--
-- >>> Apply Add (Value 3) (Apply Mul (Value 4) (Value 5))
-- <BLANKLINE>
data Expression
  -- | Wert einer Zahl
  = Value Int
  -- | Eine Rechnung bestehend aus `Operand` und den zwei
  -- Argumenten als `Expression`
  | Apply Operand Expression Expression
  deriving (Eq)

-- | alle in der `Expression` verwendeten Werte n (`Value` n)
--
-- >>> values $ Apply Add (Value 3) (Apply Mul (Value 4) (Value 5))
-- [3,4,5]
values :: Expression -> [Int]
values (Value n)     = [n]
values (Apply _ x y) = values x ++ values y

-- | Wertet eine `Expression` aus, gibt [] zurück wenn der Ausdruck
-- ungültig war, sonst wird das Ergebnis n als [n] geliefert
--
-- >>> eval $ Apply Add (Value 3) (Apply Mul (Value 4) (Value 5))
-- [23]
--
-- >>> eval $ Apply Sub (Value 3) (Value 4)
-- []
eval :: Expression -> [Int]
eval (Value n)      = [ n | n > 0 ]
eval (Apply op x y) = [ apply op a b | a <- eval x, b <- eval y, isValidOp op a b ]

bruteForceSolutions :: [Int] -> Int -> [Expression]
bruteForceSolutions ns n = [ e | ns' <- subbags ns, e <- expressions ns', eval e == [n]]

isSolution :: Expression -> [Int] -> Int -> Bool
isSolution e ns n = values e `elem` subbags ns && eval e == [n]

expressions :: [Int] -> [Expression]
expressions []  = []
expressions [n] = [Value n]
expressions ns  = [ Apply op l r | (ls,rs) <- notEmptySplit ns
                                 , l <- expressions ls
                                 , r <- expressions rs
                                 , op <- [Add, Sub, Mul, Div] ]

-- * schneller: filtere ungültige Sub-Expressions gleich heraus (siehe oben)

type Result = (Expression, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [ (Value n, n) | n > 0 ]
results ns  = [ res | (ls,rs) <- notEmptySplit ns
                    , lx <- results ls
                    , ry <- results rs
                    , res <- combine lx ry ]
  where combine (l,x) (r,y) = [ (Apply op l r, apply op x y) | op <- ops, isValidOp op x y ]
        ops = [ Add, Sub, Mul, Div ]

-- | solutions ns n berechnet alle Ausdrücke e, bei denen die `values` e
-- eine Teilmenge von ns sind und `eval` e n ergibt
solutions :: [Int] -> Int -> [Expression]
solutions ns n = [ e | ns' <- subbags ns, (e,m) <- results ns', m == n ]

-- * noch schneller: schr�nke valide Operationen mit Rechengesetzen ein:

isValidOp' :: (Ord a, Integral a) => Operand -> a -> a -> Bool
isValidOp' Add x y = x <= y
isValidOp' Sub x y = x > y
isValidOp' Mul x y = x /= 1 && y /= 1 && x <= y
isValidOp' Div x y = y /= 1 && x `mod` y == 0

results' :: [Int] -> [Result]
results' [] = []
results' [n] = [ (Value n, n) | n > 0 ]
results' ns  = [ res | (ls,rs) <- notEmptySplit ns
                     , lx <- results' ls
                     , ry <- results' rs
                     , res <- combine lx ry ]
  where combine (l,x) (r,y) = [ (Apply op l r, apply op x y) | op <- ops, isValidOp' op x y ]
        ops = [ Add, Sub, Mul, Div ]

solutions' :: [Int] -> Int -> [Expression]
solutions' ns n = [ e | ns' <- subbags ns, (e,m) <- results' ns', m == n ]


-- * Formatierung der Ausgabe / Show

instance Show Expression where
  show ex = snd $ formatEx 0 ex

formatEx :: Int -> Expression -> (Int, String)
formatEx _ (Value n) = (9, show n)
formatEx prec (Apply op l r)
  | opPrec <= prec = (prec, "(" ++ formatted ++ ")")
  | otherwise     = (prec, formatted)
  where opPrec    = precedence op
        formatted = let (lp, ls) = formatEx opPrec l
                        (_,  rs) = formatEx lp r
                    in  ls ++ show op ++ rs

precedence :: Operand -> Int
precedence Mul = 9
precedence Div = 8
precedence Add = 5
precedence Sub = 4

instance Show Operand where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
