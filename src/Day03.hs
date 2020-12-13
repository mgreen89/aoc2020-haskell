module Day03
  ( day3a
  , day3b
  )
where

tree :: (Int, String) -> Bool
tree (i, line) = line !! (i `mod` length line) == '#'

firstAndEvery :: Int -> [a] -> [a]
firstAndEvery n xs = case xs of
  (y : ys) -> y : firstAndEvery n (drop (n - 1) ys)
  []       -> []

countTrees :: (Int, Int) -> String -> Int
countTrees (xinc, yinc) =
  length . filter tree . zip [0, yinc ..] . firstAndEvery xinc . lines

day3a :: String -> Int
day3a = countTrees (1, 3)

day3b :: String -> Int
day3b input =
  let slopes = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
  in  product . fmap (`countTrees` input) $ slopes
