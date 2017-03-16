{-# LANGUAGE OverloadedStrings #-}

module Countdown.Parser
       ( tryParse
       )where

import Data.Text (Text)
import Countdown.Expressions

import Text.Parsec

tryParse :: Text -> Maybe Expression
tryParse = tryParseWith expressionP

type Parser = Parsec Text ()

expressionP :: Parser Expression
expressionP = addP

addP :: Parser Expression
addP = chainl1 multP addOp
  where addOp = choice [plusP, subP]
        plusP = (char '+' *> (return $ Apply Add))
        subP  = (char '-' *> (return $ Apply Sub))

multP :: Parser Expression
multP = chainl1 valueP multOp
  where multOp = choice [mulP,divP]
        mulP = (char '*' *> (return $ Apply Mul))
        divP = (char '/' *> (return $ Apply Div))

valueP :: Parser Expression
valueP = spaces *> (bracesP <|> numberP) <* spaces

bracesP :: Parser Expression
bracesP = between (char '(') (char ')') expressionP

numberP :: Parser Expression
numberP = Value . read <$> many1 digit

tryParseWith :: Parser a -> Text -> Maybe a
tryParseWith p txt =
  case (parse p "" txt) of
    Left _  -> Nothing
    Right a -> Just a
