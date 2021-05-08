module Hypo where

import System.Random
import Data.Word (Word64)
import Data.Bits
import Control.Monad.State.Strict

type B = Word64

data Source = MkSource
  { sprefix :: [B]
  , srecord :: [B]
  , sdraw_stack :: [Int]
  , sdraws :: [(Int, Int)]
  , srands :: [B]
  }
  deriving (Show)

mkSource :: [B] -> IO Source
mkSource prefix = do
  rands <- randoms <$> newStdGen
  return MkSource
    { sprefix = prefix
    , srecord = []
    , sdraw_stack = []
    , sdraws = []
    , srands = rands
    }

trunc n w = w .&. ((0 `setBit` n) - 1)

-- | return n bits
getbits :: Int -> Source -> (Source, B)
getbits n s | n > 64 = error "more than 64 bits requested"
getbits n s =
  let
    i = length (srecord s)
    (res, rest) =
      if i < length (sprefix s)
      then (trunc n (sprefix s !! i), srands s)
      else case srands s of
        head:tail -> (trunc n head, tail)
    s' = s
      { srands = rest
      , srecord = res : srecord s
      }
  in (s', res)

type Generator a = Source -> (Source, a)

draw :: Generator a -> Source -> (Source, a)
draw gen s =
  let
    s1 = s
      { sdraw_stack = length (srecord s) : sdraw_stack s
      }
    (s2, res) = gen s1
    s3 = s2
      { sdraws = (head (sdraw_stack s2), length (srecord s2)) : sdraws s2
      , sdraw_stack = tail (sdraw_stack s2)
      }
  in (s3, res)

data Tree
  = Branch Tree Tree
  | Leaf
  deriving (Show)

genTree :: Generator Tree
genTree s =
  case getbits 1 s of
    (s, b)
      | b == 0 -> (s, Leaf)
      | otherwise ->
        case genTree s of
          (s, left) -> case genTree s of
            (s, right) -> (s, Branch left right)


