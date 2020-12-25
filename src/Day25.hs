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

powMod :: (Integral a, Integral b) => a -> b -> a -> a
powMod x y m
  | m <= 0    = error "powMod: non-positive modulo"
  | y <  0    = error "powMod: negative exponent"
  | otherwise = f (x `rem` m) y 1 `mod` m
  where
    f _ 0 acc = acc
    f b e acc = f (b * b `rem` m) (e `quot` 2)
      (if odd e then b * acc `rem` m else acc)

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
  in  powMod doorPub cardLoopSize 20201227

day25a :: String -> Either String Int
day25a = fmap solveA . parse
