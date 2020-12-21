{-# LANGUAGE RecordWildCards #-}

module Day20
  ( day20a
  , day20b
  )
where

import           Control.Applicative            ( (<**>) )
import qualified Data.Bifunctor                as B
import           Data.Foldable                  ( foldl' )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.List                      ( transpose )
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

monster :: [String]
monster =
  ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #    "]

day20b :: String -> Either String Int
day20b i = Left "Not implemented"
