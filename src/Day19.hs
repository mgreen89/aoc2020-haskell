module Day19
  ( day19a
  , day19b
  )
where


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
  go :: Rule -> Combo Char
  go r = case r of
    Const c -> Leaf c
    Ref   c -> go' c
  go' :: Combo Int -> Combo Char
  go' c = case c of
    Leaf i  -> go $ mp IM.! i
    And  is -> And $ fmap go' is
    Or   is -> Or $ fmap go' is

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

day19a :: String -> Either String Int
day19a i = do
  (rules, msgs) <- B.first P.errorBundlePretty . P.parse inputParser "input" $ i
  let zero = expandRules rules IM.! 0
  pure $ length $ filter id $ (any null . match zero) <$> msgs


replaceRules :: IntMap Rule
replaceRules = IM.fromList
  [ (8 , Ref $ Or [Leaf 42, And [Leaf 42, Leaf 8]])
  , (11, Ref $ Or [And [Leaf 42, Leaf 31], And [Leaf 42, Leaf 11, Leaf 31]])
  ]


day19b :: String -> Either String Int
day19b i = do
  (rules, msgs) <- B.first P.errorBundlePretty . P.parse inputParser "input" $ i
  let rules = IM.union replaceRules rules
  let zero  = expandRules rules IM.! 0
  pure $ length $ filter id $ (any null . match zero) <$> msgs
