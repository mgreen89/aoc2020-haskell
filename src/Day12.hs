module Day12
  ( day12a
  , day12b
  )
where


import           Data.List                      ( foldl' )


-- Take the point definitions from the previous day.
-- there must be a better option!
-- would use copmlex numbers in python, tuple maybe better here?

type Point = (Int, Int)

add :: Point -> Point -> Point
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

scale :: Int -> Point -> Point
scale i (x, y) = (i * x, i * y)

manhattan :: Point -> Int
manhattan (x, y) = abs x + abs y

turn :: Point -> Point
turn (x, y) = (y, -x)


data Instr = Move Point
           | Turn Int
           | Forward Int
  deriving Show

makeInstr :: Char -> Int -> Instr
makeInstr c i = case c of
  'N' -> Move (0, i)
  'E' -> Move (i, 0)
  'S' -> Move (0, -i)
  'W' -> Move (-i, 0)
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
  Move    delta -> (pos `add` delta, dir)
  Forward x     -> (pos `add` scale x dir, dir)
  Turn    x     -> (pos, (!! x) . iterate turn $ dir)

day12a :: String -> Int
day12a =
  -- Initial state is a tuple of (position, direction).
  -- 'direction' is a Point to add to move in that direction.
  -- Assumes only cardinal directions.
  manhattan . fst . foldl' (flip move) ((0, 0), (1, 0)) . parse


move' :: Instr -> (Point, Point) -> (Point, Point)
move' instr (pos, wpt) = case instr of
  Move    delta -> (pos, wpt `add` delta)
  Forward x     -> (pos `add` scale x wpt, wpt)
  Turn    x     -> (pos, (!! x) . iterate turn $ wpt)

day12b :: String -> Int
day12b =
  -- Initial state is a tuple of (position, waypoint).
  manhattan . fst . foldl' (flip move') ((0, 0), (10, 1)) . parse
