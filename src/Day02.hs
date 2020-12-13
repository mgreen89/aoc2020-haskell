module Day02
  ( day2a
  , day2b
  )
where

import qualified Data.Bifunctor                as B
import           Text.Parsec                    ( many1
                                                , sepEndBy
                                                , runParser
                                                , Parsec
                                                )
import           Text.Parsec.Char               ( char
                                                , digit
                                                , letter
                                                , newline
                                                , space
                                                )

data Rule = Rule {
  low :: Int,
  high :: Int,
  check :: Char,
  pwd :: String
} deriving (Show)

-- Each line has the structure:
--   <n>-<n> <c>: <str>
--   where <n> is a digit, <c> is a char, and <str> is a string (multiple chars)
-- e.g.
--  1-3 a: bnjkdslnfa
--  7-12 q: gnjkn

ruleParser :: Parsec String () Rule
ruleParser =
  Rule
    <$> (read <$> many1 digit <* char '-')
    <*> (read <$> many1 digit <* space)
    <*> (letter <* char ':' <* space)
    <*> many1 letter

rulesParser :: Parsec String () [Rule]
rulesParser = sepEndBy ruleParser newline

parseRules :: String -> Either String [Rule]
parseRules =
  B.first show . runParser rulesParser () ""

validA :: Rule -> Bool
validA (Rule lo hi c p) =
  let cnt = length $ [ x | x <- p, x == c ] in (cnt >= lo) && (cnt <= hi)

day2a :: String -> Either String Int
day2a =
  fmap (length . filter validA) . parseRules

validB :: Rule -> Bool
validB (Rule lo hi c p) =
  let first  = (take 1 . drop (lo - 1)) p == [c]
      second = (take 1 . drop (hi - 1)) p == [c]
  in  (first && not second) || (not first && second)

day2b :: String -> Either String Int
day2b =
  fmap (length . filter validB) . parseRules
