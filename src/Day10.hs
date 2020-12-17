module Day10
  ( day10a
  , day10b
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS

freqs :: (Ord a) => [a] -> Map a Int
freqs = M.fromListWith (+) . fmap (, 1)

parse :: String -> IntSet
parse input = IS.insert (IS.findMax as + 3) as
  where as = IS.insert 0 . IS.fromList . fmap read . lines $ input

day10a :: String -> Int
day10a input =
  let adaptors = IS.toAscList $ parse input
      fs       = freqs $ zipWith (-) (drop 1 adaptors) adaptors
  in  fs M.! 1 * fs M.! 3

countWays :: IntSet -> IntMap Int
countWays as = cs
 where
  cs = flip IM.fromSet as $ \i -> if i == target
    then 1
    else sum [ IM.findWithDefault 0 (i + j) cs | j <- [1, 2, 3] ]
  target = IS.findMax as

day10b :: String -> Int
day10b = IM.findWithDefault 0 0 . countWays . parse
