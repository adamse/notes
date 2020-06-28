{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language DeriveGeneric #-}
{-# language OverloadedLabels #-}
{-# language StrictData #-}
{-# language PartialTypeSignatures #-}
{-# language NumericUnderscores #-}
{-# language FlexibleContexts #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}

{-# options -Wno-partial-type-signatures #-}
module Bs where

import GHC.Generics (Generic)
import Data.Generics.Labels

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Number.Erf

import Forward

data M a = M {spot, vol, r1, r2 :: a}
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

fwd M{..} t = spot * exp ((r2 - r1) * t)

data CallPut = Call | Put
  deriving (Show, Eq)
data O a = O {t, not, strike :: a, dir :: CallPut}
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

d1 M{..} O{..} =
  (log (spot / strike) + (r2 - r1 + (vol*vol)/2) * t)
  /
  (vol * sqrt t)

d2 m@M{..} o@O{..} = d1 m o - vol * sqrt t

price m o@O{dir} = case dir of
  Call -> price_call m o
  Put -> price_put m o

price_call :: _ => M a -> O a -> a
price_call m@M{..} o@O{..} =
  spot * exp (negate r1 * t) * normcdf (d1 m o)
  -
  strike * exp (negate r2 * t) * normcdf (d2 m o)

price_put :: _ => M a -> O a -> a
price_put m@M{..} o@O{..} =
  strike * exp (negate r2 * t) * normcdf (negate (d2 m o))
  -
  spot * exp (negate r1 * t) * normcdf (negate (d1 m o))

m0 :: _ => M a
m0 = M {spot = 1, r1 = 0, r2 = 0, vol = 10/100}

map' f = map (\x-> (x, f x))

data P a = P {ccy2pips, ccy1pct, ccy1cash, ccy2cash :: a}
  deriving (Show, Eq)

m1 :: _ => M a
m1 = m0 {spot=1.3650, r1=0.01, r2=0.02, vol=0.1}

o1 :: _ => O a
o1 = O {t = 1, strike = 1.38, not = 10000000, dir = Call}

format f m@M{..} o@O{..} =
  let
    res = f m o
    ccy2pips = res
    ccy2cash = not * ccy2pips
    ccy1pct = ccy2pips / spot
    ccy1cash = not * ccy1pct
  in P {..}

delta :: _ => (M (D a) -> O (D a) -> D a) -> M (D a) -> O (D a) -> D a
delta f m o = f (m & #spot %~ dx) o

delta_call' m@M{..} o@O{..} = exp (negate r1 * t) * normcdf (d1 m o)

vega :: _ => (M (D a) -> O (D a) -> D a) -> M (D a) -> O (D a) -> D a
vega f m o  = f (m & #vol %~ dx) o

normpdf :: _ => a -> a
normpdf x =
  exp (negate (x * x) / 2)
  /
  (sqrt (2  * pi))

vega' :: _ => M a -> O a -> a
vega' m@M{..} o@O{..} =
  spot * exp (negate r1 * t) * normpdf (d1 m o) * sqrt t

data Parms a = Parms {steps :: Int, size :: a}

spot_ladder Parms{..} f m@M{..} o = map (\scen -> (scen, f scen o)) scens
  where
    scens =
      map
        (\s -> m & #spot +~ (fromIntegral s * size))
        [(negate steps), (negate steps + 1) .. steps]


astypeof :: a -> a -> a
astypeof = const

infixr 0 `astypeof`

main :: IO ()
main =
    do
      let m = M { spot = 1, vol = 0.1, r1 = 0, r2 = 0 }
      let o = O { strike = spot m, t = 1, not = 1, dir = Call }
      print ("deltac     ", (delta price m o) :: D Double)
      print ("deltac anal", (delta_call' m o) :: D Double)
      print ("deltap     ", (delta price m o{dir=Put}) :: D Double)
      print ("vegac   ", (vega price m o) :: D Double)
      print ("vegap   ", (vega price m o{dir=Put}) :: D Double)
      print ("vegaanal", (vega' m o) :: D Double)

      print $ grad (\m -> price m o) (fmap primal m)

      putStrLn "spot_ladder price"
      mapM_ (\(scen, val) -> putStrLn ("  " <> show (spot scen ^. _1, val ^. _1)))
        (spot_ladder Parms{steps=1,size=0.025} price m o)

      putStrLn "spot_ladder delta"
      mapM_ (\(scen, val) -> putStrLn ("  " <> show (spot scen ^. _1, val ^. _2)))
        (spot_ladder Parms{steps=3,size=0.025} price (m & #spot %~ dx) o)

      putStrLn "spot_ladder vega"
      mapM_ (\(scen, val) -> putStrLn ("  " <> show (spot scen ^. _1, val ^. _2)))
        (spot_ladder Parms{steps=5,size=0.025} price (m & #vol %~ dx) o)
