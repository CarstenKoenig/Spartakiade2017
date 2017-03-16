{-# LANGUAGE OverloadedStrings #-}

module Countdown.Lists
       ( isSubsetOf
       , subbags
       , split
       , notEmptySplit
       )where

import Data.List (delete)

notEmptySplit :: [a] -> [ ([a],[a]) ]
notEmptySplit = filter notEmpty . split
  where notEmpty (xs,ys) = not (null xs || null ys)

split :: [a] -> [ ([a],[a]) ]
split []     = [ ([],[]) ]
split (x:xs) = ([], x:xs) : [ (x:ls,rs) | (ls,rs) <- split xs ]

subbags :: [a] -> [[a]]
subbags xs = [ zs | ys <- subs xs, zs <- perms ys ]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = map (x:) (subs xs) ++ subs xs

perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = [ y:ys | (y,ys') <- pick xs, ys <- perms ys' ]

pick :: [a] -> [ (a,[a]) ]
pick [] = []
pick (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- pick xs ]

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf [] _ = True
isSubsetOf _ [] = False
isSubsetOf (x:xs) ys = x `elem` ys && xs `isSubsetOf` (delete x ys)
