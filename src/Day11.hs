module Day11
  ( day11a
  , day11b
  )
where

import           Data.Foldable                  ( find )
import           Data.Maybe                     ( catMaybes
                                                , mapMaybe
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M


type Point = (Int, Int)

addPoint :: Point -> Point -> Point
addPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


parse :: String -> Map Point Bool
parse input = M.fromList $ do
  (y, row) <- zip [0 ..] $ lines input
  (x, c  ) <- zip [0 ..] row
  (,) (x, y) <$> case c of
    'L' -> pure False
    '#' -> pure True
    _   -> mempty

directNeighbours :: [Point]
directNeighbours =
  [ (dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0 ]

getNeighbours :: Point -> [Point]
getNeighbours p = fmap (addPoint p) directNeighbours

getLineOfSightNeighbours :: Map Point a -> Point -> [Point]
getLineOfSightNeighbours field point =
  let maxX = maximum . fmap fst . M.keys $ field
      maxY = maximum . fmap snd . M.keys $ field
      getNext start delta =
          find (flip M.member field)
            . takeWhile (\(x, y) -> x >= 0 && x <= maxX && y >= 0 && y <= maxY)
            . drop 1
            . iterate (addPoint delta)
            $ start
  in  mapMaybe (getNext point) directNeighbours

-- This is super slow.
generateSlow :: Int -> (Point -> [Point]) -> Map Point Bool -> Map Point Bool
generateSlow threshold getNs seats = M.mapWithKey go seats where
  go point val =
    let neighbourCount =
            length . filter id . catMaybes $ (seats M.!?) <$> (getNs point)
    in  case val of
          False -> neighbourCount == 0
          True  -> neighbourCount < threshold

-- Try passing in a mapping of neighbours since it doesn't change for each iteration
-- and using the mapping intersection which should be more efficient.
-- Don't need to do the maybe checks in this case.
-- Not actually that much faster for part a :(, but should help part b.
generate :: Int -> Map Point [Point] -> Map Point Bool -> Map Point Bool
generate threshold neighbours seats = M.intersectionWith go neighbours seats where
  go :: [Point] -> Bool -> Bool
  go ns occupied = if occupied
    then (length $ filter (seats M.!) ns) < threshold
    else not $ any (seats M.!) ns

repeatUntilSame :: Eq a => (a -> a) -> a -> a
repeatUntilSame iter start =
  let next = iter start
  in  if next == start then start else repeatUntilSame iter next

day11a :: String -> Int
day11a input =
  let start      = parse input
      neighbours = M.mapWithKey
        (\k v -> filter (flip M.member start) $ getNeighbours k)
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
