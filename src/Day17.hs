module Day17
  ( day17a
  , day17b
  )
where


import           Linear                         ( V3(..) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M


parse :: String -> Set (V3 Int)
parse =
  S.fromList
    . concat
    . zipWith (\y xs -> fmap (\(x, _) -> V3 x y 0) xs) [0 ..]
    . fmap (filter (\(x, c) -> c == '#') . zip [0 ..])
    . lines

getNeighbours :: V3 Int -> Set (V3 Int)
getNeighbours v = S.fromList
  [ v + delta | delta <- sequence (pure [-1, 0, 1]), delta /= pure 0 ]

countNeighbours :: Set (V3 Int) -> Map (V3 Int) Int
countNeighbours cubes = M.unionsWith
  (+)
  [ M.fromSet (const 1) (getNeighbours c) | c <- S.toList cubes ]

generate :: Set (V3 Int) -> Set (V3 Int)
generate cubes =
  let
    neighbourCounts = countNeighbours cubes
  in
  M.foldlWithKey' go S.empty neighbourCounts where
    go acc c ns
      | S.member c cubes = if ns == 2 || ns == 3 then S.insert c acc else acc
      | otherwise = if ns == 3 then S.insert c acc else acc

day17a :: String -> Int
day17a = S.size . (!! 6) . iterate generate . parse

day17b :: String -> Int
day17b i = 2
