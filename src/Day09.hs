module Day09
  ( day9a
  , day9b
  )
where

import Debug.Trace (trace)

import           Data.List                      ( tails )
import           Data.Maybe                     ( listToMaybe )
import           Control.Monad                  ( guard )

parse :: String -> [Int]
parse = fmap read . lines

-- Returns 'Just val' if 'val' from the end of the list invalid.
isInvalid :: [Int] -> Maybe Int
isInvalid xs =
  let tgt : srcs   = reverse xs
      invalidCheck = do
        y : ys <- tails srcs
        z      <- ys
        guard $ (y + z) == tgt
  in  if null invalidCheck then Just tgt else Nothing

findInvalid :: [Int] -> Maybe Int
findInvalid vals = listToMaybe
  [ y | ys <- tails vals, Just y <- [isInvalid (take 26 ys)] ]

day9a :: String -> Maybe Int
day9a = findInvalid . parse

day9b :: String -> Maybe Int
day9b input = do
  let vals = parse input
  invalid <- findInvalid vals
  --
  Nothing
