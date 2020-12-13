module Day01 (day1a, day1b) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet (elems, fromList, member, split)

getIntSet :: String -> IntSet
getIntSet input =
  IntSet.fromList $ map read $ lines input

day1a :: String -> IO ()
day1a input = do
  let ints = getIntSet input
  let p = [ x * y
          | x <- IntSet.elems ints
          , let (_, rest) = IntSet.split x ints
                y = 2020 - x
          , IntSet.member y rest
          ]
  print p

day1b :: String -> IO ()
day1b input = do
  let ints = getIntSet input
  let p = [ x * y * z
          | x <- IntSet.elems ints
          , let (_, rest) = IntSet.split x ints
          , y <- IntSet.elems rest
          , let (_, rest') = IntSet.split y rest
                z = 2020 - x - y
          , IntSet.member z rest'
          ]
  print p
