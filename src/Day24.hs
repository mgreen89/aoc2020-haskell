module Day24
  ( day24a
  , day24b
  ) where

import Debug.Trace
import qualified Data.Bifunctor                as B
import           Data.Foldable                  ( foldl' )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Void                      ( Void )
import           Linear                         ( V2(..) )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P

{- Hex coord system can use two coordinates.
   Picking E and NE as my two basis vectors
-}

type Coord = V2 Int

data Dir = E | SE | SW | W | NW | NE

toCoord :: Dir -> Coord
toCoord d = case d of
  E  -> V2 1 0
  SE -> V2 1 (-1)
  SW -> V2 0 (-1)
  W  -> V2 (-1) 0
  NW -> V2 (-1) 1
  NE -> V2 0 1

type Parser = P.Parsec Void String

pathParser :: Parser Coord
pathParser = sum . fmap toCoord <$> P.many
  (P.choice
    [ E <$ P.string "e"
    , SE <$ P.string "se"
    , SW <$ P.string "sw"
    , W <$ P.string "w"
    , NW <$ P.string "nw"
    , NE <$ P.string "ne"
    ]
  )

parse :: String -> Either String [Coord]
parse =
  B.first P.errorBundlePretty . traverse (P.parse pathParser "day24") . lines

findFlipped :: [Coord] -> [Coord]
findFlipped tiles =
  let flipCount =
        foldl' (\acc t -> M.unionWith (+) (M.singleton t 1) acc) M.empty tiles
  in  M.keys . M.filter odd $ flipCount

day24a :: String -> Either String Int
day24a = fmap (length . findFlipped) . parse

neighbours :: Coord -> [Coord]
neighbours init = fmap ((+ init) . toCoord) [E, SE, SW, W, NW, NE]

{- Only need to consider all white neighbours of black tiles,
   rather than the whole grid, because if a white tile has no
   black neighbours (and is therefore not a neighbour of a black
   tile) it will stay white.
-}
evolve :: Set Coord -> Set Coord
evolve blacks = S.foldl' go S.empty blacks where
  go acc black = S.unions
    ( acc
    : test black
    : (test <$> filter (not . (`S.member` blacks)) (neighbours black))
    )
  test tile =
    let blackNs = length . filter (`S.member` blacks) . neighbours $ tile
    in  if S.member tile blacks
          then if blackNs == 0 || blackNs > 2 then S.empty else S.singleton tile
          else if blackNs == 2 then S.singleton tile else S.empty

solveB :: [Coord] -> Int
solveB = S.size . (!! 100) . iterate evolve . S.fromList . findFlipped

day24b :: String -> Either String Int
day24b = fmap solveB . parse
