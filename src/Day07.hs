module Day07
  ( day7a
  , day7b
  )
where

import qualified Data.Bifunctor                as B
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L


type Bag = (String, String)

-- Graph with vertices of type v and edges of type e.
-- The rest of the solution assumes that the graph doesn't have any loops -
-- otherwise the lazy evaulation will be _|_.
type Graph v e = Map v (Map v e)

bagParser :: Parsec Void String Bag
bagParser = do
  desc   <- P.many P.lowerChar <* P.space
  colour <- P.many P.lowerChar <* P.space
  _      <- P.string "bag" <* P.optional (P.string "s")
  pure (desc, colour)

ruleParser :: Parsec Void String (Bag, Map Bag Int)
ruleParser = do
  outer  <- bagParser <* P.space <* P.string "contain" <* P.space
  inners <- P.choice
    [ P.some $ do
      n <- L.decimal <* P.space
      b <- bagParser <* P.optional (P.string "," <* P.space)
      pure (b, n)
    , do
      _ <- P.string "no other bags"
      pure []
    ]
  _ <- P.string "."
  pure (outer, M.fromList inners)

parseRules :: String -> Either String (Graph Bag Int)
parseRules =
  let rulesParser = M.fromList <$> P.sepEndBy ruleParser P.newline
  in  B.first show . P.runParser rulesParser "input"

flipGraph :: Graph Bag Int -> Graph Bag Int
flipGraph grph = M.fromListWith
  M.union
  [ (dest, M.singleton src edge)
  | (src , dests) <- M.toList grph
  , (dest, edge ) <- M.toList dests
  ]

allDescendants :: Graph Bag Int -> Map Bag (Set Bag)
allDescendants mp = descendants where
  descendants = M.foldMapWithKey mergeDescendants <$> mp
  mergeDescendants bag _ =
    S.insert bag (M.findWithDefault S.empty bag descendants)

day7a :: String -> Either String Int
day7a =
  let target = ("shiny", "gold")
  in  fmap (S.size . flip (M.!) target . allDescendants . flipGraph)
        . parseRules

countAllDescendants :: Graph Bag Int -> Map Bag Int
countAllDescendants mp = descendantCounts where
  descendantCounts = sumRecurse <$> mp
  sumRecurse inners = sum
    [ c * (M.findWithDefault 0 bag descendantCounts + 1)
    | (bag, c) <- M.toList inners
    ]

day7b :: String -> Either String Int
day7b =
  let target = ("shiny", "gold")
  in  fmap (flip (M.!) target . countAllDescendants) . parseRules
