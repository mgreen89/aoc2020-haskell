{-# LANGUAGE BangPatterns #-}

module Day25
  ( day25a
  ) where

import           Data.List                      ( find
                                                , unfoldr
                                                )

parse :: String -> Either String (Int, Int)
parse s = case lines s of
  [a, b] -> Right (read a, read b)
  _      -> Left "Invalid input"

generate :: Int -> Int -> Int
generate !subj !x = (subj * x) `rem` 20201227

loopSize :: Int -> Int
loopSize key =
  fst
    . head
    . filter ((== key) . snd)
    . zip [0 ..]
    . unfoldr (\x -> Just (x, generate 7 x))
    $ 1

solveA :: (Int, Int) -> Int
solveA (cardPub, doorPub) =
  let cardLoopSize = loopSize cardPub
  in  (!! cardLoopSize) . iterate (generate doorPub) $ 1

day25a :: String -> Either String Int
day25a = fmap solveA . parse
