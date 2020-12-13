module Day01
  ( day1a
  , day1b
  )
where

import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IntSet
                                                ( elems
                                                , fromList
                                                , member
                                                , split
                                                )

getIntSet :: String -> IntSet
getIntSet = IntSet.fromList . map read . lines

day1a :: String -> Int
day1a input =
  let ints = getIntSet input
  in  head
        [ x * y
        | x <- IntSet.elems ints
        , let (_, rest) = IntSet.split x ints
              y         = 2020 - x
        , IntSet.member y rest
        ]

day1b :: String -> Int
day1b input =
  let ints = getIntSet input
  in head
        [ x * y * z
        | x <- IntSet.elems ints
        , let (_, rest) = IntSet.split x ints
        , y <- IntSet.elems rest
        , let (_, rest') = IntSet.split y rest
              z          = 2020 - x - y
        , IntSet.member z rest'
        ]
