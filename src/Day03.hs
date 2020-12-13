module Day03
  ( day3a
  , day3b
  )
where


tree :: (Int, [Char]) -> Bool
tree (i, line) = line !! (i `mod` length line) == '#'

firstAndEvery :: Int -> [a] -> [a]
firstAndEvery n xs = case xs of
  (y : ys) -> y : firstAndEvery n (drop (n - 1) ys)
  []       -> []

countTrees :: (Int, Int) -> String -> Int
countTrees (xinc, yinc) =
  length . filter tree . zip [0, yinc ..] . firstAndEvery xinc . lines

day3a :: String -> IO ()
day3a input = do
  let trees = (length . filter tree . zip [0, 3 ..] . lines) input
  print trees
  let newtrees = countTrees (1, 3) input
  print newtrees

day3b :: String -> IO ()
day3b input = do
  let slopes       = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
  let intermediate = fmap (flip countTrees input) slopes
  print intermediate
  let ans = foldl (*) 1 intermediate
  print ans
  let ans2 = product intermediate
  print ans2

  let new = foldl (\b a -> b * (countTrees a input)) 1 slopes
  print new
