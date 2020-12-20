module Day19
  ( day19a
  , day19b
  )
where


import           Control.Monad                  ( ap )
import qualified Data.Bifunctor                as B
import           Data.Foldable                  ( foldl' )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Void                      ( Void )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L


type Parser = P.Parsec Void String

data Combo a = Leaf a
             | And [Combo a]
             | Or [Combo a]
  deriving (Show, Eq, Ord)

instance Functor Combo where
  fmap f (Leaf x ) = Leaf (f x)
  fmap f (And  xs) = And (fmap (fmap f) xs)
  fmap f (Or   xs) = Or (fmap (fmap f) xs)

instance Applicative Combo where
  pure  = Leaf
  (<*>) = ap

instance Monad Combo where
  return = pure
  (Leaf x ) >>= f = f x
  (And  xs) >>= f = And (fmap (>>= f) xs)
  (Or   xs) >>= f = Or (fmap (>>= f) xs)


data Rule = Const Char
          | Ref (Combo Int)
  deriving (Show)


constParser :: Parser Char
constParser = P.between (P.string "\"") (P.string "\"") P.lowerChar

refParser :: Parser (Combo Int)
refParser = Or <$> P.sepBy andParser (P.string "|" <* P.many (P.char ' '))
  where andParser = And <$> P.many (Leaf <$> L.decimal <* P.many (P.char ' '))

ruleParser :: Parser (Int, Rule)
ruleParser = do
  idx  <- L.decimal <* P.string ": "
  rule <- P.choice [Const <$> constParser, Ref <$> refParser]
  pure (idx, rule)

inputParser :: Parser (IntMap Rule, [String])
inputParser = do
  rules <- IM.fromList <$> P.many (ruleParser <* P.newline)
  P.newline
  msgs <- P.many P.letterChar `P.sepBy` P.newline
  pure (rules, msgs)

expandRules :: IntMap Rule -> IntMap (Combo Char)
expandRules mp = res where
  res = fmap go mp
  go r = case r of
    Const c   -> Leaf c
    Ref   cmb -> cmb >>= (res IM.!)

-- Return all tails from possible matches.
match :: Combo Char -> String -> [String]
match c s = case c of
  Leaf char -> case s of
    []     -> []
    x : xs -> [ xs | x == char ]
  And cs -> matchAll cs s
  Or  cs -> concatMap (`match` s) cs

matchAll :: [Combo Char] -> String -> [String]
matchAll cs s = foldl' go [s] cs where go poss c = concatMap (match c) poss


parse :: String -> Either String (IntMap Rule, [String])
parse = B.first P.errorBundlePretty . P.parse inputParser "day19"

solve :: IntMap Rule -> [String] -> Int
solve rules msgs =
  let zero = expandRules rules IM.! 0
  in  length . filter id . fmap (any null . match zero) $ msgs

day19a :: String -> Either String Int
day19a = fmap (uncurry solve) . parse


replaceRules :: IntMap Rule
replaceRules = IM.fromList
  [ (8 , Ref $ Or [Leaf 42, And [Leaf 42, Leaf 8]])
  , (11, Ref $ Or [And [Leaf 42, Leaf 31], And [Leaf 42, Leaf 11, Leaf 31]])
  ]

day19b :: String -> Either String Int
day19b = fmap (uncurry solve . B.first (IM.union replaceRules)) . parse
