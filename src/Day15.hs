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

day15a :: String -> Either String Int
day15a i =
  let start       = parse i
      curr : init = reverse start
      initialMap  = IM.fromList (zip (reverse init) [1 ..])
      startTurn   = length start
  in  Right . snd $ foldl' go (initialMap, curr) [startTurn .. 2019]
  where go (mp, curr) i = (IM.insert curr i mp, maybe 0 (i -) $ mp IM.!? curr)

day15b :: String -> Either String Int
day15b i = Left "Not implemented"
