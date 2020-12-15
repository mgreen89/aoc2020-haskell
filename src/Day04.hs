module Day04
  ( day4a
  , day4b
  )
where

-- Not available here :( (repl.it)
--import Data.List.Split (splitOn)
{--import Data.Map (Map, fromList)


parseEntry :: String -> Map String String
parseEntry p =
  fromList . mapMaybe . foldl m Map.empty . words where
    m x = case splitOn ":" x of
             k : v : [] -> Just (k, v)
             _ -> Nothing
--}
day4a :: String -> Either String Int
day4a input = Left "Not Implemented"

day4b :: String -> Either String Int
day4b input = Left "Not Implemented"
