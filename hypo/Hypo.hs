{-# language StandaloneDeriving #-}
{-# language DerivingStrategies #-}
{-# language DeriveFunctor #-}
{-# language BlockArguments #-}
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

newtype Gen a = Gen { runGen :: Source -> (Source, a) }

deriving stock instance Functor Gen

instance Applicative Gen where
  pure x = Gen (\s -> (s, x))
  (<*>) = ap

instance Monad Gen where
  ga >>= agb = Gen \s ->
    case runGen ga s of
      (s, a) -> runGen (agb a) s


-- | return n bits
getbits :: Int -> Gen B
getbits n | n > 64 = error "more than 64 bits requested"
getbits n = Gen \s ->
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

newtype Generator a = MkGenerator (Gen a)

mkGenerator :: Gen a -> Generator a
mkGenerator = MkGenerator

draw :: Generator a -> Gen a
draw (MkGenerator gen) = Gen \s ->
  let
    s1 = s
      { sdraw_stack = length (srecord s) : sdraw_stack s
      }
    (s2, res) = runGen gen s1
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
genTree = mkGenerator do
  b <- getbits 1
  if b == 0
    then pure Leaf
    else do
      left <- draw genTree
      right <- draw genTree
      pure (Branch left right)

sample1 :: Generator a -> IO a
sample1 g = do
  s <- mkSource []
  return (snd (runGen (draw g) s))
