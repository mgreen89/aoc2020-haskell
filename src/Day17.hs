module Day17
  ( day17a
  , day17b
  )
where

import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Linear                         ( V3(..)
                                                , V4(..)
                                                )

parse :: (Num a, Enum a, Ord b) => (a -> a -> b) -> String -> Set b
parse fn =
  S.fromList
    . concat
    . zipWith (\y xs -> fmap (\(x, _) -> fn x y) xs) [0 ..]
    . fmap (filter (\(_, c) -> c == '#') . zip [0 ..])
    . lines

getNeighbours
  :: (Ord (t a), Traversable t, Applicative t, Num a, Num (t a))
  => t a
  -> Set (t a)
getNeighbours v = S.fromList
  [ v + delta | delta <- sequence (pure [-1, 0, 1]), delta /= pure 0 ]

countNeighbours
  :: (Ord (t a), Traversable t, Applicative t, Num a, Num (t a))
  => Set (t a)
  -> Map (t a) a
countNeighbours cubes = M.unionsWith
  (+)
  [ M.fromSet (const 1) (getNeighbours c) | c <- S.toList cubes ]

generate
  :: (Ord (t a), Traversable t, Applicative t, Num a, Num (t a), Eq a)
  => Set (t a)
  -> Set (t a)
generate cubes =
  let neighbourCounts = countNeighbours cubes
  in  M.foldlWithKey' go S.empty neighbourCounts where
  go acc c ns
    | S.member c cubes = if ns == 2 || ns == 3 then S.insert c acc else acc
    | otherwise        = if ns == 3 then S.insert c acc else acc

day17a :: String -> Int
day17a = S.size . (!! 6) . iterate generate . parse (\x y -> V3 x y 0)

day17b :: String -> Int
day17b = S.size . (!! 6) . iterate generate . parse (\x y -> V4 x y 0 0)
