module Day23
  ( day23a
  , day23b
  )
where

import           Debug.Trace                    ( traceShowId )

import           Data.Sequence                  ( Seq(..)
                                                , (<|)
                                                , (|>)
                                                , (><)
                                                )
import qualified Data.Sequence                 as Seq

type Cups = Seq Int

parse :: String -> Cups
parse = Seq.fromList . fmap (read . (: []))

move :: Cups -> Cups
move start =
  let (curr :<| a :<| b :<| c :<| rest) = start
      -- Find the target.
      target                            = case Seq.filter (< curr) rest of
        Empty -> maximum rest
        xs    -> maximum xs
      -- Split on the target.
      (pre, post) = case Seq.spanl (/= target) rest of
        (xs, _ :<| ys) -> (xs, ys)
        (xs, Empty   ) -> (xs, Seq.empty)
  in  (pre |> target |> a |> b |> c) >< (post |> curr)

day23a :: String -> Either String String
day23a i =
  let finalCups  = (!! 100 ) . iterate move . parse $ i
      doubleCups = finalCups Seq.>< finalCups
  in  Right
        . concatMap show
        . Seq.takeWhileL (/= 1)
        . Seq.drop 1
        . Seq.dropWhileL (/= 1)
        $ doubleCups

day23b :: String -> Either String String
day23b i = Left "Not implemented"
