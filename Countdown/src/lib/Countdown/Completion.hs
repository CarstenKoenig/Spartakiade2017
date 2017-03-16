{-# LANGUAGE DeriveGeneric #-}

module Countdown.Completion
       ( Input (..), Expression(..)
       , start
       , next
       ) where

import Data.List (delete, (\\))
import Data.Maybe (fromJust, maybeToList)
import Countdown.Lists
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Operand
  = Add | Sub | Mul | Div
  deriving (Generic, Eq)

instance ToJSON Operand
instance FromJSON Operand

instance Show Operand where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data Expression
  = Hole
  | Value Int
  | Apply Operand Expression Expression
    deriving (Generic, Show, Eq)

instance ToJSON Expression
instance FromJSON Expression

data Input
  = Nr Int
  | Op Operand
    deriving (Generic)

instance ToJSON Input
instance FromJSON Input

instance Show Input where
  show (Nr n) = show n
  show (Op o) = show o

start :: [Int] -> [(Input, Expression)]
start ns = [ (inp, applyInput Hole inp) | inp <- possibleInputs ns Hole ]

next :: [Int] -> (Input, Expression) -> [(Input, Expression)]
next ns (_, e) = [ (inp, applyInput e inp) | inp <- possibleInputs ns e ]

applyInput :: Expression -> Input -> Expression
applyInput e (Nr n) = fromJust $ fillNextHole e (Value n)
applyInput e (Op o) = fromJust $ fillNextHole e (Apply o Hole Hole)

possibleInputs :: [Int] -> Expression -> [Input]
possibleInputs ns e = do
  (inp, next) <- nexts
  e' <- maybeToList $ fillNextHole e next
  if null (completions ns (length ns) e') then [] else [inp]
  where nexts = 
          [ (Nr n, Value n) | n <- ns ] ++ 
          [ (Op op, Apply op Hole Hole) | op <- [Add, Sub, Mul, Div], length ns - holes e > 0 ]

fillNextHole :: Expression -> Expression -> Maybe Expression
fillNextHole Hole e           = Just e
fillNextHole (Value _) _      = Nothing
fillNextHole (Apply op a b) e =
  case (fillNextHole a e, fillNextHole b e) of
    (Just e', _) -> Just $ Apply op e' b
    (_, Just e') -> Just $ Apply op a e'
    _            -> Nothing

isValidOp :: Operand -> Int -> Int -> Bool
isValidOp Add _ _ = True
isValidOp Mul _ _ = True
isValidOp Div a b = b /= 0 && a `mod` b == 0
isValidOp Sub a b = a > b

apply :: Operand -> Int -> Int -> Int
apply Add = (+)
apply Mul = (*)
apply Div = div
apply Sub = (-)

values :: Expression -> [Int]
values Hole          = []
values (Value n)     = [n]
values (Apply _ a b) = values a ++ values b

holes :: Expression -> Int
holes Hole          = 1
holes (Value _ )    = 0
holes (Apply _ a b) = holes a + holes b

eval :: Expression -> [Int]
eval Hole = []
eval (Value n) = [ n | n > 0 ]
eval (Apply op a b) = [ apply op x y | x <- eval a, y <- eval b, isValidOp op x y ]

completions :: [Int] -> Int -> Expression -> [(Expression, Int, [Int])]
completions ns m Hole = do
  ns' <- filter ((<= m) . length) $ subbags ns
  e <- expressions ns'
  v <- eval e
  let ns'' = ns \\ values e
  [(e, v, ns'')]
completions ns m e@(Value n)
  | m < 1       = []
  | n `elem` ns = [(e, n, delete n ns)]
  | otherwise = []
completions ns m (Apply op a b) = do
  let vs = values b
      m'  = m - length vs - holes b
  (ea, va, ns') <- completions ns m' a
  let taken = length ns - length ns'
  (eb, vb, ns'') <- completions ns' (m-taken) b
  if isValidOp op va vb
    then [(Apply op ea eb, apply op va vb, ns'')]
    else [] 

expressions :: [Int] -> [Expression]
expressions []  = []
expressions [n] = [Value n]
expressions ns  = [ Apply op l r | (ls,rs) <- notEmptySplit ns
                                 , l <- expressions ls
                                 , r <- expressions rs
                                 , op <- [Add, Sub, Mul, Div] ]
