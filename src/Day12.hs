module Day12
  ( day12a
  , day12b
  )
where

import           Data.Foldable                  ( foldl' )
import           Linear                         ( V2(..)
                                                , perp
                                                , (*^)
                                                )

-- Take the point definitions from the previous day.
-- there must be a better option!
-- would use copmlex numbers in python, tuple maybe better here?

type Point = V2 Int

manhattan :: Point -> Int
manhattan (V2 x y) = abs x + abs y

-- My turn is clockwise, perp turns anti-clockwise.
turn :: Point -> Point
turn = negate . perp


data Instr = Move Point
           | Turn Int
           | Forward Int
  deriving Show

makeInstr :: Char -> Int -> Instr
makeInstr c i = case c of
  'N' -> Move $ V2 0 i
  'E' -> Move $ V2 i 0
  'S' -> Move $ V2 0 (-i)
  'W' -> Move $ V2 (-i) 0
  'F' -> Forward i
  'R' -> case i of
    90  -> Turn 1
    180 -> Turn 2
    270 -> Turn 3
  'L' -> case i of
    90  -> Turn 3
    180 -> Turn 2
    270 -> Turn 1

parse :: String -> [Instr]
parse = fmap p . lines where
  p l = case l of
    x : xs -> makeInstr x (read xs)


move :: Instr -> (Point, Point) -> (Point, Point)
move instr (pos, dir) = case instr of
  Move    delta -> (pos + delta, dir)
  Forward x     -> (pos + x *^ dir, dir)
  Turn    x     -> (pos, (!! x) . iterate turn $ dir)

day12a :: String -> Int
day12a =
  -- Initial state is a tuple of (position, direction).
  -- 'direction' is a Point to add to move in that direction.
  -- Assumes only cardinal directions.
  manhattan . fst . foldl' (flip move) (V2 0 0, V2 1 0) . parse


move' :: Instr -> (Point, Point) -> (Point, Point)
move' instr (pos, wpt) = case instr of
  Move    delta -> (pos, wpt + delta)
  Forward x     -> (pos + x *^ wpt, wpt)
  Turn    x     -> (pos, (!! x) . iterate turn $ wpt)

day12b :: String -> Int
day12b =
  -- Initial state is a tuple of (position, waypoint).
  manhattan . fst . foldl' (flip move') (V2 0 0, V2 10 1) . parse
