module Day04
  ( day4a
  , day4b
  )
where

import Control.Monad (guard)
import Data.Char (isDigit)
import Data.List (isSuffixOf, span)
import           Data.List.Split                ( splitOn )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import Data.Maybe  (isJust)
import Text.Read (readMaybe)

type Passport = Map String String

list2ToTuple :: [a] -> (a, a)
list2ToTuple [a, b] = (a, b)

parsePassport :: String -> Passport
parsePassport = M.fromList . fmap (list2ToTuple . splitOn ":") . words

parse :: String -> [Passport]
parse = fmap parsePassport . splitOn "\n\n"

hasRequiredFields :: Passport -> Bool
hasRequiredFields p =
  let reqFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
  in  all (`M.member` p) reqFields

day4a :: String -> Either String Int
day4a = Right . length . filter hasRequiredFields . parse

hasValidFields :: Passport -> Bool
hasValidFields p = isJust $ do
  byr <- M.lookup "byr" p >>= readMaybe
  guard $ byr >= 1920 && byr <= 2002
  iyr <- M.lookup "iyr" p >>= readMaybe
  guard $ iyr >= 2010 && iyr <= 2020
  eyr <- M.lookup "eyr" p >>= readMaybe
  guard $ eyr >= 2020 && eyr <= 2030
  hgt <- M.lookup "hgt" p
  guard $ isJust $ case span isDigit hgt of
    (num@(x: xs), suf@(y: ys)) ->
      case suf of
        "in" -> do
          hgtNum <- readMaybe num
          guard $ hgtNum >= 59 && hgtNum <= 76
        "cm" -> do
          hgtNum <- readMaybe num
          guard $ hgtNum >= 150 && hgtNum <= 193
        _ -> Nothing
    _ -> Nothing
  hcl <- M.lookup "hcl" p
  guard $ take 1 hcl == "#"
  let allowedHex = "0123456789abcdef"
  guard $ all (\c -> any (== c) allowedHex) (drop 1 hcl)
  ecl <- M.lookup "ecl" p
  guard $ any (== ecl) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  pid <- M.lookup "pid" p
  guard $ length pid == 9
  (readMaybe pid :: Maybe Int)

day4b :: String -> Either String Int
day4b = Right . length . filter hasValidFields . parse
