{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NumericUnderscores #-}
{-# language PartialTypeSignatures #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}

{-# options -Wall #-}
module Forward where

import Control.Lens.Combinators
import GHC.Generics (Generic)
import Data.Traversable (mapAccumL)

import Data.Number.Erf

data D a = D !a a
  deriving (Show, Generic, Eq, Ord, Functor, Foldable, Traversable)

-- instance Applicative D where
--   pure x = D x x
--   D f g <*> D x y =
instance Field1 (D a) (D a) a a
instance Field2 (D a) (D a) a a

-- chain: D \x -> f (g x) = \x -> f' (g x) * g' x

chain1 :: Num a => (a -> a) -> (a -> a) -> D a -> D a
chain1 f f' (D gx g'x) = D (f gx) (f' gx * g'x)

auto :: Num a => a -> D a
auto x = D x 0

bundle :: a -> a -> D a
bundle = D

primal :: D a -> a
primal (D a _) = a

tangent :: D a -> a
tangent (D _ a) = a

instance Num a => Num (D a) where
  {-# specialise instance Num (D Double) #-}

  fromInteger x = auto (fromInteger x)

  D a1 a2 + D b1 b2 = D (a1 + b1) (a2 + b2)

  D a1 a2 * D b1 b2 = D (a1 * b1) (a1 * b2 + a2 * b1)

  abs = undefined -- chain1 abs signum

  signum = undefined -- chain1 signum (const 0)

  negate = chain1 negate (const (-1))


instance Fractional a => Fractional (D a) where
  {-# specialise instance Fractional (D Double) #-}

  fromRational x = auto (fromRational x)

  recip = chain1 recip (negate . (\gx -> recip (gx * gx)))

instance Real a => Real (D a) where
  toRational (D a _) = toRational a

instance Floating a => Floating (D a) where
  {-# specialise instance Floating (D Double) #-}

  pi = auto pi

  exp = chain1 exp exp

  log = chain1 log recip

  sin = chain1 sin cos

  cos = chain1 cos (negate . sin)

  asin = chain1 asin (\x -> recip (sqrt (1 - x * x)))

  acos = chain1 acos (\x -> negate (recip (sqrt (1 - x * x))))

  atan = chain1 tan (\x -> recip (x * x + 1))

  sinh = chain1 sinh cosh

  cosh = chain1 cosh sinh

  asinh = chain1 asinh (recip . (\x -> sqrt (x * x + 1)))

  acosh = chain1 acosh (recip . (\x -> sqrt (x * x - 1)))

  atanh = chain1 atanh (recip . (\x -> 1 - x * x))

instance Erf a => Erf (D a) where
  {-# specialise instance Erf (D Double) #-}

  erf = chain1 erf (\x -> (2 / sqrt pi) * exp (negate x * x))
  erfc = chain1 erfc (\x -> ((-2) / sqrt pi) * exp (negate x * x))
  normcdf = chain1 normcdf (\x -> (recip (sqrt (2 * pi))) * exp (negate x * x / 2))

---

table1 :: Num a => (D a -> D a) -> [a] -> [(a, a)]
table1 f = map ((\(D a b) -> (a, b)) . f . flip bundle 1)

diff :: Num a => (D a -> D a) -> a -> a
diff f x = case f (D x 1) of
  D _ f'x -> f'x

diff' :: Num a => (D a -> D a) -> a -> (a, a)
diff' f x = case f (D x 1) of
  D fx f'x -> (fx, f'x)

dx :: Num a => D a -> D a
dx (D a _) = D a 1

grad
  :: (Traversable f, Num a)
  => (f (D a) -> D b)
  -> f a
  -> (b, f b)
-- this version is pretty much what you find in ekmett/ad
grad f as = mk (mapAccumL outer (0 :: Int, b0) as)
  where
    mk ((_, b), bs) = (b, bs)
    b0 = primal (f (auto <$> as))
    outer (row, _) _ =
      let D b b' = f (snd (mapAccumL (inner row) 0 as)) in
        ( (row + 1, b)
        , b'
        )
    inner row col val =
      ( col + 1
      , if col == row then bundle val 1 else auto val
      )
