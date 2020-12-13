module Day06
  ( day6a
  , day6b
  )
where

import           Data.Bits                      ( popCount
                                                , shiftL
                                                , (.|.)
                                                , (.&.)
                                                )
import           Data.Char                      ( ord )
import           Data.List                      ( sort )

-- In ASCII a = 97, b = 98, c = 99 etc... therefore just
-- subtract 97 to get 0, 1, 2 etc...
-- Can use this to populate a bitfield for the lowercase alphabet.
lineToBitField :: String -> Int
lineToBitField = foldl f 0 where f acc c = acc .|. 1 `shiftL` (ord c - 97)

-- Can use the fact that lineToBitField will return 0 on a empty line to
-- work out when each entry has finished.
--
-- N.B. This means we need to ensure there's _exactly_ one empty row at
-- the end of the input (i.e. it ends "<last-item>\n\n").
-- (part 1 is fine with multiple empty rows, part 2 is not)
common :: (Int -> Int -> Int) -> Int -> String -> Int
common combine initial input =
  let bfs = lineToBitField <$> lines input
  in  sum $ snd $ foldl f (initial, []) bfs where
  f (current, groups) bf = case bf of
    0 -> (initial, popCount current : groups)
    x -> (combine x current, groups)

day6a :: String -> Int
day6a = common (.|.) 0

day6b :: String -> Int
day6b =
  -- Initial value -1 to start with all 1 bits.
  common (.&.) (-1)
