module Day21
  ( day21a
  , day21b
  )
where

import qualified Data.Bifunctor                as B
import           Data.Foldable                  ( foldl' )
import           Data.List                      ( intercalate
                                                , sortOn
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Void                      ( Void )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = P.Parsec Void String

foodParser :: Parser (Set String, Set String)
foodParser =
  (,)
    <$> (S.fromList <$> ingredientsParser)
    <*> (S.fromList <$> allergensParser) where
  ingredientsParser = P.sepEndBy (P.some P.lowerChar) (P.char ' ')
  allergensParser   = P.between
    (P.char '(')
    (P.char ')')
    (P.string "contains " *> P.sepBy (P.some P.lowerChar) (P.string ", "))

parse :: String -> Either String [(Set String, Set String)]
parse =
  traverse (B.first P.errorBundlePretty . P.parse foodParser "day20") . lines

{- Takes the input foods and returns matched [(ingredient, allergen)].
-}
findAllergens :: [(Set String, Set String)] -> [(String, String)]
findAllergens foods =
  let
    -- Construct a mapping of allergen to possible ingredients.
      allergenMap = foldl' go M.empty foods         where
          go acc (ins, als) =
            M.unionWith S.intersection (M.fromSet (const ins) als) acc

      -- Loop over a list of (allergen, possible ingredients) tuples.
      -- Produces a list of matched (ingredient, allergen) tuples.
      loop candidates = case break ((== 1) . S.size . snd) candidates of
        (pre, (a, is) : rem) ->
          -- Just checked this, but must be a better way than calling head!
          let ingr = head $ S.toList is
          -- Sneaky use of a the 2-tuple functor that only changes the
          -- second element.
          in  (ingr, a) : loop (fmap (S.delete ingr) <$> (pre ++ rem))
        _ -> []
  in  loop $ M.toList allergenMap

solveA :: [(Set String, Set String)] -> Int
solveA foods = sum $ fmap go foods where
  go (is, as) = S.size . S.filter (\i -> not $ S.member i allergenIngrs) $ is
  allergenIngrs = S.fromList . fmap fst . findAllergens $ foods

day21a :: String -> Either String String
day21a = fmap (show . solveA) . parse

solveB :: [(Set String, Set String)] -> String
solveB = intercalate "," . fmap fst . sortOn snd . findAllergens

day21b :: String -> Either String String
day21b = fmap solveB . parse
