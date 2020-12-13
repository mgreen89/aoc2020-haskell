module Day04
  ( day4a
  , day4b
  )
where

-- Not available here :(
--import Data.List.Split (splitOn)
{--import Data.Map (Map, fromList)


parseEntry :: String -> Map String String
parseEntry p =
  fromList . mapMaybe . foldl m Map.empty . words where
    m x = case splitOn ":" x of
             k : v : [] -> Just (k, v)
             _ -> Nothing
--}
day4a :: String -> IO ()
day4a input = do
  --let inputs = splitOn "\n\n" input
  --let passports = fmap parseEntry inputs
  --print (take 1 passports)
  print "day4a"

day4b :: String -> IO ()
day4b input = do
  print "day4b"
