module Day22
  ( day22a
  , day22b
  )
where

import qualified Data.Bifunctor                as B
import           Data.Foldable                  ( toList )
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

play :: Deck -> Deck -> Deck
play one two = case (one, two) of
  (x :<| xs, y :<| ys) ->
    if x > y then play (xs :|> x :|> y) ys else play xs (ys :|> y :|> x)
  (_    , Empty) -> one
  (Empty, _    ) -> two

day22a :: String -> Either String Int
day22a = fmap (score . uncurry play) . parse

day22b :: String -> Either String Int
day22b i = Left "Not implemented"
