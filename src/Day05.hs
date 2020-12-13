module Day05
  ( day5a
  , day5b
  )
where

import           Control.Monad                  ( foldM )
import           Data.Bits                      ( shiftL )
import           Data.List                      ( sort )

lineToId :: String -> Either String Int
lineToId s = foldM go 0 s where
  go acc c = case c of
    x | x `elem` "FL" -> Right $ acc `shiftL` 1
      | x `elem` "BR" -> Right $ (acc `shiftL` 1) + 1
      | otherwise     -> Left $ "Invalid char: " ++ [x]

day5a :: String -> Either String Int
day5a = fmap maximum . traverse lineToId . lines

day5b :: String -> Either String Int
day5b input = do
  sortedIds <- sort <$> (mapM lineToId . lines) input
  let sortedIds' = tail sortedIds
  let missingList = foldl go [] $ zip sortedIds sortedIds'
        where go acc (a, a') = if a' == a + 1 then acc else a + 1 : acc

  case missingList of
    x : [] -> Right x
    a ->
      Left ("Invalid data - got invalid missing numbers: " ++ show missingList)
