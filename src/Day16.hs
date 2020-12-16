{-# LANGUAGE RecordWildCards #-}

module Day16
  ( day16a
  , day16b
  )
where

import           Debug.Trace                    ( trace )

import qualified Data.Bifunctor                as B
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Void                      ( Void )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L

type Ticket = [Int]

data FieldRange = FR { frLo1 :: Int
                     , frHi1 :: Int
                     , frLo2 :: Int
                     , frHi2 :: Int
                     }
  deriving (Show)

data Info = I { iFields :: Map String FieldRange
              , iMine :: Ticket
              , iOthers :: [Ticket]
              }
  deriving (Show)

fieldParser :: P.Parsec Void String (String, FieldRange)
fieldParser = do
  field <- P.many (P.choice [P.lowerChar, P.satisfy (== ' ')])
  P.string ": "
  frLo1 <- L.decimal
  P.string "-"
  frHi1 <- L.decimal
  P.string " or "
  frLo2 <- L.decimal
  P.string "-"
  frHi2 <- L.decimal
  P.newline
  pure (field, FR { .. })

ticketParser :: P.Parsec Void String Ticket
ticketParser = P.sepBy L.decimal (P.string ",")

infoParser :: P.Parsec Void String Info
infoParser = do
  iFields <- M.fromList <$> P.many fieldParser
  P.string "\nyour ticket:\n"
  iMine <- ticketParser <* P.newline
  P.string "\nnearby tickets:\n"
  iOthers <- P.sepEndBy ticketParser P.newline
  pure I { .. }

parseInfo :: String -> Either String Info
parseInfo = B.first P.errorBundlePretty . P.parse infoParser "input"

validFieldValue :: Int -> FieldRange -> Bool
validFieldValue v f  =
  (v >= frLo1 f && v <= frHi1 f) || (v >= frLo2 f && v <= frHi2 f)

day16a :: String -> Either String Int
day16a i = do
  info <- parseInfo i
  Right $ sum
    [ n
    | n <- concat (iOthers info)
    , not $ any (validFieldValue n) (M.elems $ iFields info)
    ]

day16b :: String -> Either String Int
day16b i =
  Left "Not implemented"
