module Day18
  ( day18a
  , day18b
  ) where

import           Control.Monad.Combinators.Expr ( makeExprParser
                                                , Operator(..)
                                                )
import qualified Data.Bifunctor                as B
import           Data.Void                      ( Void )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = P.Parsec Void String

exprParser :: [[Operator Parser Int]] -> Parser Int
exprParser ops = makeExprParser (term ops) ops

term :: [[Operator Parser Int]] -> Parser Int
term ops =
  P.choice [L.decimal, P.between (P.string "(") (P.string ")") (exprParser ops)]
    <* P.space

partAOps :: [[Operator Parser Int]]
partAOps =
  [ [ InfixL $ (+) <$ P.string "+" <* P.space
    , InfixL $ (*) <$ P.string "*" <* P.space
    ]
  ]

partBOps :: [[Operator Parser Int]]
partBOps =
  [ [InfixL $ (+) <$ P.string "+" <* P.space]
  , [InfixL $ (*) <$ P.string "*" <* P.space]
  ]

parse :: [[Operator Parser Int]] -> String -> Either String Int
parse ops = B.first P.errorBundlePretty . P.parse (exprParser ops) "input"

day18a :: String -> Either String Int
day18a = fmap sum . traverse (parse partAOps) . lines

day18b :: String -> Either String Int
day18b = fmap sum . traverse (parse partBOps) . lines
