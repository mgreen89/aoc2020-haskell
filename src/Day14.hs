module Day14
  ( day14a
  , day14b
  )
where

import qualified Data.Bifunctor                as B
import           Data.Bits                      ( (.|.)
                                                , (.&.)
                                                )
import           Data.Char                      ( digitToInt )
import           Data.Foldable                  ( foldl' )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.Void                      ( Void )
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L


{-
The masking can be done as two bitwise operations:
  - a bitwise & where all the X elements are 1.
  - a bitwise | where all the X elements are 0.
-}

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

replaceXWith :: Char -> String -> String
replaceXWith r input =
  let replace 'X' = r
      replace c   = c
  in  fmap replace input


data Instr = Mask (Int, Int)
           | Mem (Int, Int)
  deriving (Show)

instrParser :: P.Parsec Void String Instr
instrParser = P.choice
  [ Mask <$> do
    _ <- P.string "mask = "
    b <- P.many P.alphaNumChar
    pure (toDec . replaceXWith '0' $ b, toDec . replaceXWith '1' $ b)
  , Mem <$> do
    _    <- P.string "mem["
    addr <- L.decimal
    _    <- P.string "] = "
    val  <- L.decimal
    pure (addr, val)
  ]

parseInstrs :: String -> Either String [Instr]
parseInstrs = B.first P.errorBundlePretty
  . P.parse (P.sepEndBy instrParser P.newline) "input"

day14a :: String -> Either String Int
day14a = fmap (sum . snd . foldl' go ((0, -1), IM.empty)) . parseInstrs where
  go (currMask, im) instr = case instr of
    Mask (orMask, andMask) -> ((orMask, andMask), im)
    Mem (loc, val) ->
      (currMask, IM.insert loc ((val .|. fst currMask) .&. snd currMask) im)

day14b :: String -> Either String Int
day14b i = Left "Not implemented"
