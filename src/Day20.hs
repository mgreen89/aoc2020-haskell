{-# LANGUAGE RecordWildCards #-}

module Day20
  ( day20a
  , day20b
  )
where

import qualified Data.Bifunctor as B
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
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
inputParser =
  IM.fromList <$> P.sepBy tileParser P.newline

parse :: String -> Either String (IntMap Tile)
parse =
  B.first P.errorBundlePretty . P.parse inputParser "day20"

day20a :: String -> Either String Int
day20a i = Left "Not implemented"

day20b :: String -> Either String Int
day20b i = Left "Not implemented"
