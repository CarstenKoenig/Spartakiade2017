{-# LANGUAGE OverloadedStrings #-}

module Countdown.Game.Random
       ( baseNumbers
       , generateChallange
       )where


import Control.Monad.Random
import Data.List (delete, sort)
import System.Random

import Countdown.Game.Challanges
import Countdown.Expressions

baseNumbers :: [Int]
baseNumbers = [1..10] ++ [1..10] ++ [15, 25, 50, 75, 100]

generateChallange :: IO Challange
generateChallange = evalRandIO randomChallange

-- | Versucht einen zufällige Herausforderung zu finden
-- 
-- Zunächst werden 6 zufällige Zahlen aus den Vorrat gezogen
-- mit denen dann versucht wird einen gültigen Ausdruck mit
-- Wert zwischen 100 und 999 zu finden
--
-- Schlägt das 10 Mal fehl beginnt der Algorithmus erneut
-- mit den Ziehen von 6 zufälligen Zahlen
randomChallange :: RandomGen g => Rand g Challange
randomChallange = do
  ns <- randomPickN 6 baseNumbers
  goal <- findValid 10 ns
  case goal of
    Just g  -> return $ Challange g (sort ns)
    Nothing -> randomChallange
  where
    findValid 0 _ = return Nothing
    findValid repick ns = do
      ex <- randomExpression ns
      case fromValid (eval ex) of
        Just n
          | imZielbereich n -> return (Just n)
          | otherwise       -> findValid (repick - 1) ns
        Nothing             -> findValid (repick - 1) ns
    fromValid [n] = Just n
    fromValid _   = Nothing
    imZielbereich n = n >= 100 && n < 1000

randomExpression :: RandomGen g => [Int] -> Rand g Expression
randomExpression ns = do
  n <- getRandomR (3, 6)
  let xs = take n ns
  rndSplit xs
  where
    rndSplit [] = error "cannot split on empty list"
    rndSplit [n] = return $ Value n
    rndSplit ns = do
      i <- getRandomR (1, length ns-1)
      op <- uniform ops
      let (ls,rs) = (take i ns, drop i ns)
      ls' <- rndSplit ls
      rs' <- rndSplit rs
      return $ Apply op ls' rs'
    ops = [Add, Sub, Mul, Div]


randomPickN :: (Eq a, RandomGen g) => Int -> [a] -> Rand g [a]
randomPickN n xs
  | n <= 0 = return []
  | otherwise = do
    x'  <- uniform xs
    xs' <- randomPickN (n-1) (delete x' xs)
    return (x':xs')

