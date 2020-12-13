module Day13
  ( day13a
  , day13b
  )
where

import           Debug.Trace                    ( trace )

import           Data.List                      ( foldl1'
                                                , minimumBy
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Ord                       ( comparing )


parsea :: String -> Either String (Int, [Int])
parsea input = case splitOn "\n" input of
  tgt : ids : [] ->
    Right (read tgt, fmap read . filter (/= "x") . splitOn "," $ ids)
  i -> Left $ "Invalid input: " ++ show i


day13a :: String -> Either String Int
day13a input = do
  (target, ids) <- parsea input
  Right
    . uncurry (*)
    . minimumBy (comparing snd)
    $ [ (id, wait) | id <- ids, let wait = (id - target `rem` id) ]


parseb :: String -> Either String [(Int, Int)]
parseb input = case splitOn "\n" input of
  _ : ids : [] ->
    Right
      . fmap (\(a, b) -> (a, read b))
      . filter ((/= "x") . snd)
      . zip [0 ..]
      . splitOn ","
      $ ids
  i -> Left $ "Invalid input: " ++ show i


{-
Stating with the example, first three entries are (0, 7), (1, 13), (4, 59).

These restrictions can be stated as:

  1)  t `rem` 7 == 0
  2)  (t + 1) `rem` 13 == 0
  3)  (t + 4) `rem` 59 == 0

Obviously iterating to over 100 trillion is not an option.
Is there a short cut? (yes... obviously)

Starting with just the first two, can triviall find the answer 77:

  77 `rem` 7 == 0 (./)
  (77 + 1) `rem` 13 == 0 (./)

  Unfortunately
  (77 + 4) `rem` 59 /= 0 (X) :(

  However, we can massively restrict the number of checks we need to make:
    only need to try 77 + (lowest-common-multiple 7 13) * N

  Very helpfully, all the bus numbers are prime, so we can just use
  (7 * 13) as the interval to check.

  Then, once we've found the number that matches 59, we can just check
    new match + (7 * 13 * 59) * N.
-}

day13b :: String -> Either String Int
day13b = fmap (fst . foldl1' go) . parseb where
  go (match, step) (offset, busId) =
    ( head
      . filter (\x -> (x + offset) `rem` busId == 0)
      . iterate ((+) step)
      $ match
    , step * busId
    )
