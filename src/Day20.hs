{-# LANGUAGE RecordWildCards #-}

module Day20
  ( day20a
  , day20b
  ) where

import qualified Data.Bifunctor                as B
import           Data.Bits                      ( (.&.) )
import           Data.Bool                      ( bool )
import           Data.Foldable                  ( foldl' )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.List                      ( foldl1'
                                                , transpose
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Void                      ( Void )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = P.Parsec Void String

type Tile = [String]

data Transform = Tr Bool Int
  deriving (Show, Eq, Ord)

tileParser :: Parser (Int, Tile)
tileParser = do
  tId      <- P.string "Tile " *> L.decimal <* P.char ':' <* P.space
  tContent <- P.sepEndBy (P.some (P.choice [P.char '.', P.char '#'])) P.newline
  pure (tId, tContent)

inputParser :: Parser (IntMap Tile)
inputParser = IM.fromList <$> P.sepBy tileParser P.newline

parse :: String -> Either String (IntMap Tile)
parse = B.first P.errorBundlePretty . P.parse inputParser "day20"

-- A rotation is a reverse and a transpose.
rotate :: Int -> [[a]] -> [[a]]
rotate n = (!! n) . iterate (transpose . reverse)

transforms :: [Transform]
transforms = [ Tr flipped rots | flipped <- [False, True], rots <- [0 .. 3] ]

transform :: Transform -> [[a]] -> [[a]]
transform (Tr False n) = rotate n
transform (Tr True  n) = rotate n . reverse

getTransforms :: [[a]] -> [[[a]]]
getTransforms t = [ transform tr t | tr <- transforms ]

getBorders :: [[a]] -> [[a]]
getBorders = fmap head . getTransforms

-- Get a map of top border to (tile id, tile contents)
getBorderMap :: IntMap Tile -> Map String [(Int, [String])]
getBorderMap =
  IM.foldlWithKey' go M.empty     where
    go acc tId t = M.unionWith
      (++)
      (M.fromList [ (head tC, [(tId, tC)]) | tC <- getTransforms t ])
      acc

findCorners :: IntMap Tile -> [Int]
findCorners mp =
  let
    {- Find all the corners by finding all possible edges.
       Then find the edges that are only present on one tile.
       Then, for each tile that has any of those edges, find the tiles
       that have _two_ unique edges - this will only happen at the corners.
    -}
    borderMap = getBorderMap mp
    uniqueEdges = M.filter ((== 1) . length) borderMap

    -- Get a map from tId to unique edges.
    idToUnique = M.foldlWithKey' go M.empty uniqueEdges where
      go a b ((tId, _) : []) = M.unionWith (++) (M.singleton tId [b]) a

    -- Find all the IDs with 4 unique edges (which is really
    -- 2 edges * 2 orientations).
    corners = M.keys . M.filter ((== 4) . length) $ idToUnique
  in corners

{- Part A.
   Don't actually need to bother constructing the full map - just need to
   find the corners.
-}
day20a :: String -> Either String Int
day20a = fmap (product . findCorners) . parse

getSig :: [String] -> Int
getSig =
  foldl' (\a x -> a * 2 + bool 0 1 (x == '#')) 0 . concatMap (take 20) . take 3

monster :: [String]
monster =
  ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #    "]

monsterSig :: Int
monsterSig = getSig monster

findMonsters :: [String] -> Int
findMonsters ss = findMonsters' ss ss where
  findMonsters' ss0 ss
    | length ss < 3
    = 0
    | otherwise
    = (if getSig ss .&. monsterSig == monsterSig then 1 else 0)
      + if length (head ss) > 20
          then findMonsters' ss0 (map tail ss)
          else findMonsters (tail ss0)

solveB mp =
  let
    borderMap = getBorderMap mp

    startId    = head (findCorners mp)
    startContent  = mp IM.! startId

    -- Want to orient the tile s.t. the borders with other tiles are on the
    -- bottom and right.
    -- Can check if the border is shared by looking in the border map:
    --   if the border maps to one tile, it's not shared.
    --   if the border maps to two tiles, it's shared.
    -- Since we don't really care about flipping orientation:
    --  if the border on the top matches, flip the tile vertically.
    --  if the corder on the left matches, flip the tile horizontally.
    startTileTfs =
      [ if length (borderMap M.! head startContent) == 2 then reverse else id
      , if length (borderMap M.! (head . transpose $ startContent)) == 2
        then fmap reverse
        else id
      ]

    startTile = foldl' (.) id startTileTfs startContent

    belowTiles (tId, tC) =
      case filter ((/= tId) . fst) $ borderMap M.! last tC of
        [next] -> (tId, tC) : belowTiles next
        _      -> [(tId, tC)]

    -- Right tiles is just bottom tiles with a transpose before and after.
    rightTiles (tId, tC) = transpose . snd <$> belowTiles (tId, transpose tC)

    -- Stick 'em all together!
    full          = fmap rightTiles . belowTiles $ (startId, startTile)

    -- Remove the borders.
    mid           = tail . init
    borderless    = fmap (fmap (fmap mid . mid)) full

    -- Concatentate the rows.
    grid          = concatMap (foldl1' (zipWith (++))) borderless

    -- Find monsters in all orientations.
    -- Then take the max as they should all be zero except one.
    monsterCounts = fmap findMonsters (getTransforms grid)
    monsterCount = maximum monsterCounts

    -- Count the occurence of '#'
    countHits :: [String] -> Int
    countHits = length . filter (== '#') . concat
  in
    countHits grid - (monsterCount * countHits monster)

day20b :: String -> Either String Int
day20b = fmap solveB . parse
