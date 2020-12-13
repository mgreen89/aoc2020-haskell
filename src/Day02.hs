module Day02
  ( day2a
  , day2b
  )
where

import           Text.Parsec                    ( many1
                                                , sepEndBy
                                                , runParser
                                                , ParsecT
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

ruleParser :: (Monad m) => ParsecT String u m Rule
ruleParser =
  Rule
    <$> (read <$> many1 digit <* char '-')
    <*> (read <$> many1 digit <* space)
    <*> (letter <* char ':' <* space)
    <*> (many1 letter)

rulesParser :: (Monad m) => ParsecT String u m [Rule]
rulesParser = sepEndBy ruleParser newline

validA :: Rule -> Bool
validA (Rule lo hi c p) =
  let cnt = length $ [ x | x <- p, x == c ] in (cnt >= lo) && (cnt <= hi)

day2a :: String -> IO ()
day2a input = do
  let rules      = runParser rulesParser () "" input
  let validrules = fmap (length . filter validA) rules
  print validrules

validB :: Rule -> Bool
validB (Rule lo hi c p) =
  let first  = (take 1 . drop (lo - 1)) p == [c]
      second = (take 1 . drop (hi - 1)) p == [c]
  in  (first && not second) || (not first && second)

day2b :: String -> IO ()
day2b input = do
  let rules      = runParser rulesParser () "" input
  let validrules = fmap (length . filter validB) rules
  print validrules
