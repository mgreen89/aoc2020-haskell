module Lib
  ( libmain
  )
where

import           Day01                          ( day1a
                                                , day1b
                                                )
import           Day02                          ( day2a
                                                , day2b
                                                )
import           Day03                          ( day3a
                                                , day3b
                                                )
import           Day04                          ( day4a
                                                , day4b
                                                )
import           Day05                          ( day5a
                                                , day5b
                                                )
import           Day06                          ( day6a
                                                , day6b
                                                )
import           Day07                          ( day7a
                                                , day7b
                                                )
import           Day08                          ( day8a
                                                , day8b
                                                )
import           Day09                          ( day9a
                                                , day9b
                                                )
import           Day10                          ( day10a
                                                , day10b
                                                )
import           Day11                          ( day11a
                                                , day11b
                                                )
import           Day12                          ( day12a
                                                , day12b
                                                )
import           Day13                          ( day13a
                                                , day13b
                                                )
import           Day14                          ( day14a
                                                , day14b
                                                )
import           Day15                          ( day15a
                                                , day15b
                                                )
import           Day16                          ( day16a
                                                , day16b
                                                )


import           Control.DeepSeq                ( NFData )
import           Control.Monad                  ( (<=<)
                                                , when
                                                )
import           Control.Monad.Fail             ( MonadFail )
import           Criterion
import           Data.Maybe                     ( mapMaybe )
import           System.Environment             ( getArgs )
import           Text.Printf
import           Text.Read                      ( readMaybe )

getDayInput :: Int -> IO String
getDayInput i = readFile filename
  where filename = printf "inputs/day%0.2d.txt" i

checkRight :: MonadFail m => Either String a -> m a
checkRight = either fail return

checkJust :: MonadFail m => Maybe a -> m a
checkJust = maybe (fail "Failed!") return

runDay :: NFData a => Int -> (a -> IO ()) -> [String -> a] -> IO ()
runDay day output parts = do
  days <- mapMaybe readMaybe <$> getArgs
  when (null days || day `elem` days) $ do
    putStrLn $ "Day " ++ show day
    contents <- getDayInput day
    --putStrLn "Answers:"
    mapM_ (output . ($ contents)) parts
    --putStrLn "\nBenchmarks:"
    --mapM_ (\p -> benchmark (whnf p contents)) parts
    putStrLn ""

libmain :: IO ()
libmain = do
  runDay 1  print                  [day1a, day1b]
  runDay 2  (print <=< checkRight) [day2a, day2b]
  runDay 3  print                  [day3a, day3b]
  runDay 4  (print <=< checkRight) [day4a, day4b] -- Not done day 4 (not really my jam)
  runDay 5  (print <=< checkRight) [day5a, day5b]
  runDay 6  print                  [day6a, day6b]
  runDay 7  (print <=< checkRight) [day7a, day7b]
  runDay 8  (print <=< checkRight) [day8a, day8b]
  runDay 9  (print <=< checkJust)  [day9a, day9b]
  runDay 10 print                  [day10a, day10b]
  runDay 11 print                  [day11a, day11b]
  runDay 12 print                  [day12a, day12b]
  runDay 13 (print <=< checkRight) [day13a, day13b]
  runDay 14 (print <=< checkRight) [day14a, day14b]
  runDay 15 print                  [day15a, day15b]
  runDay 16 (print <=< checkRight) [day16a, day16b]
