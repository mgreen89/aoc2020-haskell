module Day23
  ( day23a
  , day23b
  )
where

import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable                  ( toList )
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IM
import           Data.Sequence                  ( Seq(..)
                                                , (<|)
                                                , (|>)
                                                , (><)
                                                )
import qualified Data.Sequence                 as Seq
import qualified Data.Vector.Unboxed.Mutable   as V

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
  let finalCups  = (!! 100) . iterate move . parse $ i
      doubleCups = finalCups >< finalCups
  in  Right
        . concatMap show
        . Seq.takeWhileL (/= 1)
        . Seq.drop 1
        . Seq.dropWhileL (/= 1)
        $ doubleCups

{-
  So, I'd love to have a proper linked list for this, and maybe pointer
  manipulation in a State monad would be quicker, but to start with
  let's just simulate a linked list by using a IntMap, where the value
  for each key is the right-neighbour.
-}

topB :: Int
topB = 1000000

itersB :: Int
itersB = 10000000

moveB :: Int -> IntMap Int -> IntMap Int
moveB curr mp =
  let a   = mp IM.! curr
      b   = mp IM.! a
      c   = mp IM.! b
      d   = mp IM.! c
      tgt = until (\x -> x /= 0 && x /= a && x /= b && x /= c)
                  (\x -> if x <= 1 then topB else x - 1)
                  (curr - 1)
      tgtNext = mp IM.! tgt
  in  IM.union (IM.fromList [(tgt, a), (c, tgtNext), (curr, d)]) mp

solveB :: Cups -> Int
solveB parsed =
  let pList            = toList parsed
      (_     :|> last) = parsed
      (first :<| _   ) = parsed
      maxIn            = maximum pList

      initial          = IM.unions
        [ IM.fromList (zip pList (drop 1 pList))
        , IM.fromList (zip (last : [maxIn + 1 .. topB]) [maxIn + 1 .. topB])
        , IM.singleton topB first
        ]

      finished = snd $ foldl go (first, initial) [1 .. itersB]
          where go (c, mp) _ = let next = moveB c mp in (next IM.! c, next)

      ans1 = finished IM.! 1
      ans2 = finished IM.! ans1
  in  ans1 * ans2

{- Let's try the implementation with a mutable vector in the state monad. -}
moveB' :: Int -> V.MVector s Int -> ST s Int
moveB' curr v = do
  a <- V.read v curr
  b <- V.read v a
  c <- V.read v b
  d <- V.read v c
  let tgt = until (\x -> x /= 0 && x /= a && x /= b && x /= c)
                  (\x -> if x <= 1 then topB else x - 1)
                  (curr - 1)
  tgtNext <- V.read v tgt
  V.write v curr d
  V.write v tgt a
  V.write v c tgtNext
  return d

runB' :: Int -> Int -> V.MVector s Int -> ST s ()
runB' 0     _ _ = return ()
runB' iters c v = do
  n <- moveB' c v
  runB' (iters - 1) n v

solveB' :: Cups -> Int
solveB' parsed =
  let pList            = toList parsed
      (_     :|> last) = parsed
      (first :<| _   ) = parsed
      maxIn            = maximum pList
  in  runST $ do
        v <- V.new (topB + 1)
        forM_ (zip pList (drop 1 pList)) (uncurry $ V.write v)
        forM_ (zip (last : [maxIn + 1 .. topB]) [maxIn + 1 .. topB])
              (uncurry $ V.write v)
        V.write v topB first
        runB' itersB first v
        ans1 <- V.read v 1
        ans2 <- V.read v ans1
        return $ ans1 * ans2

day23b :: String -> Either String String
day23b = Right . show . solveB' . parse
