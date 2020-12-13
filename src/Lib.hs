module Lib (libmain) where

import Day01 (day1a, day1b)
import Day02 (day2a, day2b)
import Day03 (day3a, day3b)
import Day04 (day4a, day4b)
import Day05 (day5a, day5b)
import Day06 (day6a, day6b)
import Day07 (day7a, day7b)
import Day08 (day8a, day8b)
import Day09 (day9a, day9b)
import Day10 (day10a, day10b)
import Day11 (day11a, day11b)
import Day12 (day12a, day12b)
import Day13 (day13a, day13b)


import Control.Monad ((<=<), when)
import Control.Monad.Fail (MonadFail)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Printf
import Text.Read (readMaybe)

getDayInput :: Int -> IO String
getDayInput i =
  readFile filename
  where
    filename = printf "inputs/day%0.2d.txt" i

checkRight :: MonadFail m => Either String a -> m a
checkRight = either fail return

runDay :: Int -> (a -> IO ()) -> [String -> a] -> IO ()
runDay day output parts = do
  days <- mapMaybe readMaybe <$> getArgs
  when (null days || day `elem` days) $ do
    putStrLn $ "Day " ++ show day
    contents <- getDayInput day
    mapM_ (output . ($ contents)) parts
    putStrLn ""

libmain :: IO ()
libmain = do
  runDay 1 id [ day1a, day1b ]
  runDay 2 id [ day2a, day2b ]
  runDay 3 id [ day3a, day3b ]
  runDay 4 id [ day4a, day4b ] -- Not done day 4 (not really my jam)
  runDay 5 (print <=< checkRight) [ day5a, day5b ]
  runDay 6 print [ day6a, day6b ]
  runDay 7 print [ day7a, day7b ] -- Not done (vectors)
  runDay 8 (print <=< checkRight) [ day8a, day8b ]
  runDay 9 print [ day9a, day9b ] -- Not done (vectors)
  runDay 10 print [ day10a, day10b ]
  runDay 11 print [ day11a, day11b ] -- (takes a while on repl.it)
  runDay 12 print [ day12a, day12b ]
  runDay 13 (print <=< checkRight) [ day13a, day13b ]
