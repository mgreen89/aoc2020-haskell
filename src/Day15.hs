module Day15
  ( day15a
  , day15b
  )
where

import           Control.Monad.Loops            ( whileM_ )
import           Control.Monad.Primitive        ( PrimState )
import           Control.Monad.State.Strict     ( evalStateT
                                                , get
                                                , gets
                                                , put
                                                , StateT
                                                )
import           Control.Monad.ST               ( runST
                                                , ST
                                                )
import           Data.Foldable                  ( foldl'
                                                , for_
                                                )
import Data.Int (Int32)
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.List.Split                ( splitOn )
import qualified Data.Vector.Unboxed.Mutable   as MV


parse :: String -> [Int]
parse = fmap read . splitOn ","

run :: Int -> [Int] -> Int
run upTo initial =
  let curr : init = reverse initial
      initialMap  = IM.fromList (zip (reverse init) [1 ..])
      startTurn   = length initial
  in  snd $ foldl' go (initialMap, curr) [startTurn .. (upTo - 1)]
  where go (mp, curr) i = (IM.insert curr i mp, maybe 0 (i -) $ mp IM.!? curr)

day15a :: String -> Int
day15a = run 2020 . parse


data LoopState = LS { lsIdx :: !Int32 , lsLast :: !Int }

getNext :: MV.MVector s Int32 -> StateT LoopState (ST s) ()
getNext vec = do
  LS idx last <- get
  lastSeen    <- MV.unsafeRead vec last
  MV.unsafeWrite vec last idx
  let next = if lastSeen == 0 then 0 else idx - lastSeen
  put $ LS (idx + 1) (fromIntegral next)
{-# INLINE getNext #-}

setInitial :: MV.MVector s Int32 -> Int -> StateT LoopState (ST s) ()
setInitial vec val = do
  LS idx last <- get
  MV.unsafeWrite vec last idx
  put $ LS (idx + 1) val
{-# INLINE setInitial #-}

runMutable :: Int -> [Int] -> Int
runMutable upTo initial = runST $ flip evalStateT (LS 0 0) $ do
  v <- MV.replicate upTo 0
  for_ initial (setInitial v)
  whileM_ (gets ((< upTo32) . lsIdx)) (getNext v)
  gets lsLast
  where
    upTo32 :: Int32
    upTo32 = fromIntegral upTo

day15b :: String -> Int
day15b = runMutable 30000000 . parse
