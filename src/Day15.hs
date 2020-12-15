module Day15
  ( day15a
  , day15b
  )
where

import           Data.Foldable                  ( foldl' )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.List.Split                ( splitOn )


parse :: String -> [Int]
parse = fmap read . splitOn ","

run :: Int -> [Int] -> Int
run upTo initial =
  let curr : init = reverse initial
      initialMap  = IM.fromList (zip (reverse init) [1 ..])
      startTurn   = length initial
  in  snd $ foldl' go (initialMap, curr) [startTurn .. (upTo - 1)]
  where go (mp, curr) i = (IM.insert curr i mp, maybe 0 (i -) $ mp IM.!? curr)

day15a :: String -> Int
day15a = run 2020 . parse

day15b :: String -> Int
day15b = run 30000000 . parse
