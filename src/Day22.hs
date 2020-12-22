module Day22
  ( day22a
  , day22b
  )
where

import qualified Data.Bifunctor                as B
import           Data.Foldable                  ( toList )
import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet                  as HS
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Seq
import           Data.Void                      ( Void )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L

type Deck = Seq Int

type Parser = P.Parsec Void String

inputParser :: Parser (Deck, Deck)
inputParser = (,) <$> deckParser <*> deckParser where
  deckParser = do
    P.string "Player " *> L.decimal <* P.string ":" <* P.newline
    Seq.fromList <$> P.many (L.decimal <* P.newline) <* P.many P.newline

parse :: String -> Either String (Deck, Deck)
parse = B.first P.errorBundlePretty . P.parse inputParser "day22"

score :: Deck -> Int
score = sum . zipWith (*) [1 ..] . reverse . toList

playA :: Deck -> Deck -> Deck
playA one two = case (one, two) of
  (x :<| xs, y :<| ys) ->
    if x > y then playA (xs :|> x :|> y) ys else playA xs (ys :|> y :|> x)
  (_    , Empty) -> one
  (Empty, _    ) -> two

day22a :: String -> Either String Int
day22a = fmap (score . uncurry playA) . parse


takeExactly :: Int -> Seq a -> Maybe (Seq a)
takeExactly n xs | Seq.length xs >= n = Just $ Seq.take n xs
                 | otherwise          = Nothing

data Player = P1 | P2
  deriving (Show, Eq)

playB :: HashSet ([Int], [Int]) -> Deck -> Deck -> (Player, Deck)
playB seen one two
  | HS.member (toList one, toList two) seen = (P1, one)
  | otherwise = case (one, two) of
    (x :<| xs, y :<| ys) ->
      let recurse = do
            xs' <- takeExactly x xs
            ys' <- takeExactly y ys
            pure . fst $ playB HS.empty xs' ys'
          winner = case recurse of
            Just p  -> p
            Nothing -> if x > y then P1 else P2
          seen' = HS.insert (toList one, toList two) seen
      in  case winner of
            P1 -> playB seen' (xs :|> x :|> y) ys
            P2 -> playB seen' xs (ys :|> y :|> x)
    (Empty, _    ) -> (P2, two)
    (_    , Empty) -> (P1, one)

day22b :: String -> Either String Int
day22b = fmap (score . snd . uncurry (playB HS.empty)) . parse
