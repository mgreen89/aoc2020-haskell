module Day09
  ( day9a
  , day9b
  )
where

import           Debug.Trace                    ( trace )

import           Data.List                      ( tails )
import           Data.Maybe                     ( listToMaybe )
import qualified Data.Vector                   as V
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
findInvalid vals =
  listToMaybe [ y | ys <- tails vals, Just y <- [isInvalid (take 26 ys)] ]

day9a :: String -> Maybe Int
day9a = findInvalid . parse

day9b :: String -> Maybe Int
day9b input = do
  let vals = parse input
  invalid <- findInvalid vals
  let valvec = V.fromList vals
      ans    = go (0, 0, valvec V.! 0)       where
        go (i, j, tot) = case compare tot invalid of
          LT -> go (i, j + 1, tot + valvec V.! (j + 1))
          EQ ->
            let slice = V.slice i (j - i + 1) valvec
            in  (minimum slice + maximum slice)
          GT -> go (i + 1, j, tot - valvec V.! i)
  pure ans
