module Day14
  ( day14a
  , day14b
  )
where

import           Control.Monad                  ( foldM )
import qualified Data.Bifunctor                as B
import           Data.Bits                      ( clearBit
                                                , setBit
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
The masking for part a can be done as two bitwise operations:
  - a bitwise & where all the X elements are 1.
  - a bitwise | where all the X elements are 0.

Part b is a bit more complex, so reworked the mask to be a list of
'Maybe Bool', where:
  - Just True  : the bit should be overridden 1
  - Just False : the bit should be overridden to 0
  - Nothing    : the bit shouldn't be changed (part a) or is floating (part b)
-}


data Instr = Mask [Maybe Bool]
           | Mem (Int, Int)
  deriving (Show)

instrParser :: P.Parsec Void String Instr
instrParser = P.choice
  [ Mask <$> do
    _    <- P.string "mask = "
    mask <- P.many
      (P.choice
        [ Just True <$ P.char '1'
        , Just False <$ P.char '0'
        , Nothing <$ P.char 'X'
        ]
      )
      -- Mask list needs reversing so the list goes from lowest to highest
      -- value bit.
    pure (reverse mask)
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

mask :: Int -> [Maybe Bool] -> Int
mask v = foldl' go v . zip [0 ..] where
  go v (idx, mb) = case mb of
    Just True  -> setBit v idx
    Just False -> clearBit v idx
    Nothing    -> v

day14a :: String -> Either String Int
day14a = fmap (sum . snd . foldl' go ([], IM.empty)) . parseInstrs where
  go (currMask, im) instr = case instr of
    Mask newMask    -> (newMask, im)
    Mem  (loc, val) -> (currMask, IM.insert loc (mask val currMask) im)


-- Use the list monad as a "choice"
mask' :: Int -> [Maybe Bool] -> [Int]
mask' v = foldM go v . zip [0 ..] where
  go v (idx, mb) = case mb of
    Just True  -> [setBit v idx]
    Just False -> [v]
    Nothing    -> [setBit v idx, clearBit v idx]

day14b :: String -> Either String Int
day14b = fmap (sum . snd . foldl' go ([], IM.empty)) . parseInstrs where
  go (currMask, im) instr = case instr of
    Mask newMask -> (newMask, im)
    Mem (loc, val) ->
      let newMem = IM.fromList ((\a -> (a, val)) <$> mask' loc currMask)
      in  (currMask, IM.union newMem im)
