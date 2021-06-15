{-# LANGUAGE MultiParamTypeClasses #-}
{-# language TupleSections #-}
{-# language StandaloneDeriving #-}
{-# language DerivingStrategies #-}
{-# language DeriveFunctor #-}
{-# language BlockArguments #-}
{-# language OverloadedLists #-}
{-# language TemplateHaskell #-}
{-# language LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}
module Hypo where

import qualified Data.List as List
import System.Random
import Data.Word (Word64)
import Data.Bits
import Control.Monad.State.Strict
import Data.Vector (Vector)
import qualified Data.Vector as Vec

type B = Word64

data Stream a = !a :> Stream a

makeStream :: [a] -> Stream a
makeStream (a:as) = a :> makeStream as
makeStream _ = error "makeStream: empty list :("

unsnoc :: Stream a -> (a, Stream a)
unsnoc (a:>as) = (a, as)

data Source = MkSource
  { _prefix :: Vector B
  -- ^ prefix of random bits
  , _record :: [B]
  , _draw_stack :: [Int]
  , _draws :: [(Int, Int)]
  , _rands :: Stream B
  -- ^ stream of random bits
  }

mkSource :: Vector B -> IO Source
mkSource prefix = do
  rands <- randoms <$> newStdGen
  return MkSource
    { _prefix = prefix
    , _record = []
    , _draw_stack = []
    , _draws = []
    , _rands = makeStream rands
    }

trunc :: Int -> B -> B
trunc n w = w .&. ((0 `setBit` n) - 1)

newtype Gen a = Gen { runGen :: Source -> (a, Source) }

deriving stock instance Functor Gen

instance Applicative Gen where
  pure x = Gen (x,)
  (<*>) = ap

instance Monad Gen where
  ga >>= agb = Gen \s ->
    case runGen ga s of
      (a, s) -> runGen (agb a) s

instance MonadState Source Gen where state = Gen

-- | return n bits
getBits :: Int -> Gen B
getBits n | n > 64 = error "more than 64 bits requested"
getBits n = do
  i <- List.length <$> gets _record
  pref <- gets _prefix
  res <- trunc n <$> case pref Vec.!? i of
    Just res -> pure res
    Nothing -> do
      (res, rest) <- gets (unsnoc . _rands)
      modify \s -> s { _rands = rest }
      pure res
  modify \s -> s { _record = res : _record s }
  pure res

newtype Generator a = MkGenerator (Gen a)

mkGenerator :: Gen a -> Generator a
mkGenerator = MkGenerator

draw :: Generator a -> Gen a
draw (MkGenerator gen) = do
  modify \s -> s
     { _draw_stack = length (_record s) : _draw_stack s }
  res <- gen
  modify \s -> s
     { _draws = (head (_draw_stack s), length (_record s)) : _draws s
     , _draw_stack = tail (_draw_stack s) }
  pure res

data Tree
  = Branch Tree Tree
  | Leaf
  deriving (Show)

genTree :: Generator Tree
genTree = mkGenerator do
  b <- getBits 1
  if b == 0
    then pure Leaf
    else Branch <$> draw genTree <*> draw genTree

sample1 :: Generator a -> IO a
sample1 g = do
  s <- mkSource []
  return (fst (runGen (draw g) s))
