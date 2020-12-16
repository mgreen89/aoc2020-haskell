{-# LANGUAGE RecordWildCards #-}

module Day16
  ( day16a
  , day16b
  )
where

import qualified Data.Bifunctor                as B
import           Data.List                      ( foldl1' )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import qualified Data.Vector                   as V
import           Data.Void                      ( Void )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L

type Ticket = [Int]

data FieldRange = FR { frLo1 :: Int
                     , frHi1 :: Int
                     , frLo2 :: Int
                     , frHi2 :: Int
                     }
  deriving (Show)

data Info = I { iFields :: Map String FieldRange
              , iMine :: Ticket
              , iOthers :: [Ticket]
              }
  deriving (Show)

fieldParser :: P.Parsec Void String (String, FieldRange)
fieldParser = do
  field <- P.many (P.choice [P.lowerChar, P.satisfy (== ' ')])
  P.string ": "
  frLo1 <- L.decimal
  P.string "-"
  frHi1 <- L.decimal
  P.string " or "
  frLo2 <- L.decimal
  P.string "-"
  frHi2 <- L.decimal
  P.newline
  pure (field, FR { .. })

ticketParser :: P.Parsec Void String Ticket
ticketParser = P.sepBy L.decimal (P.string ",")

infoParser :: P.Parsec Void String Info
infoParser = do
  iFields <- M.fromList <$> P.many fieldParser
  P.string "\nyour ticket:\n"
  iMine <- ticketParser <* P.newline
  P.string "\nnearby tickets:\n"
  iOthers <- P.sepEndBy ticketParser P.newline
  pure I { .. }

parseInfo :: String -> Either String Info
parseInfo = B.first P.errorBundlePretty . P.parse infoParser "input"

validFieldValue :: Int -> FieldRange -> Bool
validFieldValue v f =
  (v >= frLo1 f && v <= frHi1 f) || (v >= frLo2 f && v <= frHi2 f)

day16a :: String -> Either String Int
day16a i = do
  info <- parseInfo i
  Right $ sum
    [ n
    | n <- concat (iOthers info)
    , not $ any (validFieldValue n) (M.elems $ iFields info)
    ]

day16b :: String -> Either String Int
day16b i = do
  info <- parseInfo i

  let
    -- Filter the other tickets by validity.
    ticketValid  = all (\t -> any (validFieldValue t) (iFields info))
    validTickets = iMine info : filter ticketValid (iOthers info)

    -- Get a set of all field names for which a given value is valid.
    possibleFields v =
      S.fromList
        $ [ f | (f, fr) <- M.toList $ iFields info, validFieldValue v fr ]

    {-
      Looper function:
        Takes a list of tuple of (field index, possible field names), and
        finds the first tuple where the set of possible field names has
        _exactly_ one entry (so doesn't necessarily work when you just have
        to pick one and try it out, but apparently no need for that).

        Once it's found that entry, removes that field name from all
        the other entries in the list, and iterates, adding that field
        number/name combo to the output list.

        Outputs a list of (field index, field name) tuples.
    -}
    loop candidates = case break ((== 1) . S.size . snd) candidates of
      (pref, (i, names) : rem) ->
        let name = head $ S.toList names
          -- Just checked this, but must be a better way than calling head!
        in  (i, name) : loop (fmap (S.delete name) <$> (pref ++ rem))
      _ -> []

    -- Find the sets of all possible fields for all valid tickets.
    allPossibleFields =
      fmap possibleFields <$> validTickets

    -- Combine the sets generated above into one set per field.
    zippedPossibleFields =
      foldl1' (zipWith S.intersection) allPossibleFields

    -- Loop over the sets and get a list of (field index, field name) tuples.
    fieldIndexes =
      loop . zip [0 ..] $ zippedPossibleFields

    -- Turn my ticket into a vector for easy lookup.
    mine = V.fromList (iMine info)

    -- Get the values of all fields starting with "departure".
    departureFields =
      [ mine V.! i
      | (i, name) <-fieldIndexes
      , take 9 name == "departure"
      ]

  pure $ product departureFields
