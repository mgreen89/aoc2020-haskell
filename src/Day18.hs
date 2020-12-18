module Day18
  ( day18a
  , day18b
  )
where

import qualified Data.Bifunctor                as B
import           Data.Void                      ( Void )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L


type Parser = P.Parsec Void String


-- Parser that eats (and discards) any following whitespace.
eatWs :: Parser a -> Parser a
eatWs p = p <* P.space

{- Part A -}
parseInt :: Parser Int
parseInt = eatWs
  $ P.choice [L.decimal, P.between (P.string "(") (P.string ")") parseExpr]

parseExpr :: Parser Int
parseExpr = do
  left <- parseInt
  iter left
 where
  iter acc = P.choice
    [ do
      op    <- eatWs $ P.choice [(*) <$ P.string "*", (+) <$ P.string "+"]
      right <- parseInt
      iter $ op acc right
    , pure acc
    ]

parseA :: String -> Either String Int
parseA = B.first P.errorBundlePretty . P.parse parseExpr "input"

day18a :: String -> Either String Int
day18a = fmap sum . traverse parseA . lines

{- Part B
   Need another level in the parser to deal with operator precedence.

   TODO: Commonise!
-}

parseBot :: Parser Int
parseBot =
  eatWs $ P.choice [L.decimal, P.between (P.string "(") (P.string ")") parseTop]

parseMid :: Parser Int
parseMid = do
  left <- parseBot
  iter left
 where
  iter acc = P.choice
    [ do
      op    <- eatWs $ (+) <$ P.string "+"
      right <- parseBot
      iter $ op acc right
    , pure acc
    ]

parseTop :: Parser Int
parseTop = do
  left <- parseMid
  iter left
 where
  iter acc = P.choice
    [ do
      op    <- eatWs $ (*) <$ P.string "*"
      right <- parseMid
      iter $ op acc right
    , pure acc
    ]

parseB :: String -> Either String Int
parseB = B.first P.errorBundlePretty . P.parse parseTop "input"

day18b :: String -> Either String Int
day18b = fmap sum . traverse parseB . lines
