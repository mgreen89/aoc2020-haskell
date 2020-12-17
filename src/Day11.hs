module Day11
  ( day11a
  , day11b
  )
where

import           Control.Lens
import           Data.Foldable                  ( find )
import           Data.Maybe                     ( catMaybes
                                                , mapMaybe
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Linear                         ( R1(..)
                                                , R2(..)
                                                , V2(..)
                                                )

type Point = V2 Int

parse :: String -> Map Point Bool
parse input = M.fromList $ do
  (y, row) <- zip [0 ..] $ lines input
  (x, c  ) <- zip [0 ..] row
  (,) (V2 x y) <$> case c of
    'L' -> pure False
    '#' -> pure True
    _   -> mempty

directNeighbours :: [Point]
directNeighbours =
  [ delta | delta <- sequence (pure [-1, 0, 1]), delta /= pure 0 ]

getNeighbours :: Point -> [Point]
getNeighbours p = fmap (+ p) directNeighbours

getLineOfSightNeighbours :: Map Point a -> Point -> [Point]
getLineOfSightNeighbours field point =
  let maxX = maximum . fmap (^. _x) . M.keys $ field
      maxY = maximum . fmap (^. _y) . M.keys $ field
      getNext start delta =
          find (`M.member` field)
            . takeWhile (\(V2 x y) -> x >= 0 && x <= maxX && y >= 0 && y <= maxY)
            . drop 1
            . iterate (+ delta)
            $ start
  in  mapMaybe (getNext point) directNeighbours

-- This is super slow.
generateSlow :: Int -> (Point -> [Point]) -> Map Point Bool -> Map Point Bool
generateSlow threshold getNs seats = M.mapWithKey go seats where
  go point val =
    let neighbourCount =
            length . filter id . catMaybes $ (seats M.!?) <$> getNs point
    in  if val then neighbourCount == 0 else neighbourCount < threshold

-- Try passing in a mapping of neighbours since it doesn't change for each iteration
-- and using the mapping intersection which should be more efficient.
-- Don't need to do the maybe checks in this case.
-- Not actually that much faster for part a :(, but should help part b.
generate :: Int -> Map Point [Point] -> Map Point Bool -> Map Point Bool
generate threshold neighbours seats = M.intersectionWith go neighbours seats where
  go :: [Point] -> Bool -> Bool
  go ns occupied = if occupied
    then length (filter (seats M.!) ns) < threshold
    else not $ any (seats M.!) ns

repeatUntilSame :: Eq a => (a -> a) -> a -> a
repeatUntilSame iter start =
  let next = iter start
  in  if next == start then start else repeatUntilSame iter next

day11a :: String -> Int
day11a input =
  let start      = parse input
      neighbours = M.mapWithKey
        (\k v -> filter (`M.member` start) $ getNeighbours k)
        start
      final = repeatUntilSame (generate 4 neighbours) start
  in  length . filter id . fmap snd . M.toList $ final

day11b :: String -> Int
day11b input =
  let start = parse input
      neighbours =
          M.mapWithKey (\k v -> getLineOfSightNeighbours start k) start
      final = repeatUntilSame (generate 5 neighbours) start
  in  length . filter id . fmap snd . M.toList $ final
