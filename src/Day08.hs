module Day08
  ( day8a
  , day8b
  )
where

import           Debug.Trace                    ( trace )

import qualified Data.Bifunctor                as B
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.List                      ( foldl' )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.Void                      ( Void )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L


data Instr = NOP Int
           | ACC Int
           | JMP Int
  deriving (Eq, Ord, Show)

instrParser :: P.Parsec Void String Instr
instrParser = P.choice
  [ NOP <$> (P.string "nop" *> P.space *> L.signed P.space L.decimal)
  , ACC <$> (P.string "acc" *> P.space *> L.signed P.space L.decimal)
  , JMP <$> (P.string "jmp" *> P.space *> L.signed P.space L.decimal)
  ]

progParser :: P.Parsec Void String (Vector Instr)
progParser = V.fromList <$> P.sepEndBy instrParser P.newline

parseProg :: String -> Either String (Vector Instr)
parseProg = B.first P.errorBundlePretty . P.parse progParser "input"


data Machine = Machine { pc :: !Int, acc :: !Int }
  deriving (Eq, Show)

initialMachine :: Machine
initialMachine = Machine 0 0

step :: Vector Instr -> Machine -> Maybe Machine
step prog state = do
  cmd <- prog V.!? pc state
  Just $ case cmd of
    NOP _ -> state { pc = pc state + 1 }
    ACC x -> state { pc = pc state + 1, acc = acc state + x }
    JMP x -> state { pc = pc state + x }


data EndType = Halt | Loop
  deriving (Eq, Ord, Show)


run' :: Vector Instr -> IntSet -> Machine -> (EndType, Machine)
run' prog seen state = case step prog state of
  Just next -> if IS.member (pc state) seen
    then (Loop, state)
    else run' prog (IS.insert (pc state) seen) next
  Nothing -> (Halt, state)

run :: Vector Instr -> (EndType, Machine)
run prog = run' prog IS.empty initialMachine


day8a :: String -> Either String Int
day8a input = do
  prog <- parseProg input
  case run prog of
    (Halt, s) -> Left "Unexpectedly halted!"
    (Loop, s) -> pure $ acc s


day8b :: String -> Either String Int
day8b =
  -- Create a list of perturbed vectors
  let perturbs prog = snd $ V.foldl go (0, []) prog         where
          go (i, l) _ = case prog V.! i of
            NOP x -> (i + 1, (prog V.// [(i, JMP x)]) : l)
            ACC x -> (i + 1, l)
            JMP x -> (i + 1, (prog V.// [(i, NOP x)]) : l)
  in  fmap (acc . head . foldl' go [] . perturbs) . parseProg where
  go l p = case run p of
    (Halt, s) -> s : l
    (Loop, s) -> l
