{-# LANGUAGE RecordWildCards #-}

module Day20
  ( day20a
  , day20b
  ) where

import           Control.Applicative            ( (<**>) )
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

newtype Tile = Tile { tContent :: [String] }
  deriving (Show)

tileParser :: Parser (Int, Tile)
tileParser = do
  tId      <- P.string "Tile " *> L.decimal <* P.char ':' <* P.space
  tContent <- P.sepEndBy (P.some (P.choice [P.char '.', P.char '#'])) P.newline
  pure (tId, Tile { .. })

inputParser :: Parser (IntMap Tile)
inputParser = IM.fromList <$> P.sepBy tileParser P.newline

parse :: String -> Either String (IntMap Tile)
parse = B.first P.errorBundlePretty . P.parse inputParser "day20"

getBorders :: Tile -> [String]
getBorders (Tile c) =
  -- <**> is (flip <*>)
  -- `last` is not particularly efficient but list are only 12 long
  [c] <**> [id, transpose] <**> [head, last] <**> [id, reverse]

solveA :: IntMap Tile -> [Int]
solveA mp =
  {- Don't actually need to bother constructing the full map.
     Instead, find all possible borders for each tile, and count all
     the appearances of each possibility. Assuming all joined borders are
     unique, all the possibilites that are actually borders will appear
     twice - once for each tile they appear on, and will appear forwards
     and backwards.
     Then iterate through the tiles again, finding the only four tiles
     that have only two borders (i.e. four entries - two borders both
     forward and backward remember) - these are the corners.
  -}
  let allPossibilities = foldl'
        (\acc s -> M.unionWith (+) (M.singleton s 1) acc)
        M.empty
        (concatMap (getBorders . snd) . IM.toList $ mp)

      idToCount = IM.mapWithKey go mp         where
          go i t = foldl' go' 0 (getBorders t)
          go' acc p = if allPossibilities M.! p == 2 then acc + 1 else acc
  in  [ i | i <- (fmap fst . IM.toList) mp, idToCount IM.! i == 4 ]

day20a :: String -> Either String Int
day20a = fmap (product . solveA) . parse

data Transform = Tr Bool Int
  deriving (Show, Eq, Ord)

-- A rotation is a reverse and a transpose.
rotate :: Int -> [[a]] -> [[a]]
rotate n = (!! n) . iterate (transpose . reverse)

transforms :: [Transform]
transforms = [ Tr flipped rots | flipped <- [False, True], rots <- [0 .. 3] ]

transform :: Transform -> [[a]] -> [[a]]
transform (Tr False n) = rotate n
transform (Tr True  n) = rotate n . reverse

getTransforms t = [ transform tr t | tr <- transforms ]

getSig :: [String] -> Int
getSig =
  foldl' (\a x -> a * 2 + bool 0 1 (x == '#')) 0 . concatMap (take 20) . take 3

monster :: [String]
monster =
  ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #    "]

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
    borderMap :: Map String [(Int, [String])]
    borderMap = IM.foldlWithKey' go M.empty mp     where
      go acc tId (Tile t) = M.unionWith
        (++)
        (M.fromList [ (head tC, [(tId, tC)]) | tC <- getTransforms t ])
        acc

    -- Whose stupid idea was the record! :(
    startId    = head (solveA mp)
    (Tile stc) = mp IM.! startId

    -- Want to orient the tile s.t. the borders with other tiles are on the bottom and right.
    -- Can check if the border is shared by looking in the border map:
    --   if the border maps to on tile, it's not shared.
    --   if the border maps to two tiles, it's shared.
    -- Since we don't really care about flipping orientation:
    --  if the border on the top matches, flip the tile vertically.
    --  if the corder on the left matches, flip the tile horizontally.
    startTileTfs =
      [ if length (borderMap M.! head stc) == 2 then reverse else id
      , if length (borderMap M.! (head . transpose $ stc)) == 2
        then fmap reverse
        else id
      ]

    startTile = foldl' (.) id startTileTfs stc

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
