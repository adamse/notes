{-# language NamedFieldPuns #-}
{-# language DeriveGeneric #-}
{-# language OverloadedLabels #-}
module Options1 where

import GHC.Generics
import Data.Generics.Labels

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Number.Erf

import Chart

import Forward

data I a = I { t, r1, r2, vol :: a }
  deriving (Show, Eq, Generic)

i0 :: I Double
i0 = I {t = 1, r1 = 0.05/100, r2 = 0.02/100, vol = 8/100}
spot0 = 1.3360

exp_ret I{t, r1, r2, vol} = (r2 - r1 - 0.5 * vol * vol) * t
stddev I{t, vol} = vol * sqrt t

ret_level i x = exp_ret i + x * stddev i
spot_level i spot x = spot * exp (ret_level i x)

prob x1 x2 = normcdf x2 - normcdf x1

-- number of std devs
xs :: [Double]
xs = [(-5.0), (-4.9) .. 5]

terminal_spot_plot = do
  layout_title .= ("Terminal spot distribution spot = " <> show spot0)
  layout_all_font_styles . font_size %= (2 *)
  layout_x_axis . laxis_title .= "Spot"
  layout_y_axis . laxis_title .= "Probability density"
  let spot_levels i = map (\(x1, x2) -> (spot_level i spot0 x1, prob x1 x2)) (zip xs (tail xs))
  mapM_ (\(nm, i) -> plot (line nm [spot_levels i]))
    [ (show i0, i0)
    , ("shorter time", i0 & #t *~ 0.5) -- shorter time -> tighter
    , ("higher vol", i0 & #vol *~ 2) -- higher vol -> wider
    , ("higher ccy2 rate", i0 & #r2 +~ (10/100)) -- higher ccy2 rate -> move to right (higher)
    ]

call_payoff spot strike = max (spot - strike) 0
put_payoff spot strike = max (strike - spot) 0

strike = 1.3600

option_payoff_plot = do
  layout_title .= ("Option payoff")
  layout_all_font_styles . font_size %= (2 *)
  layout_x_axis . laxis_title .= "Spot"
  layout_y_axis . laxis_title .= "Payoff, CCY2 pips"

  let spot_levels = map (spot_level i0 spot0) xs
  plot (line ("Call, strike = " <> show strike) [map (\s -> (s, call_payoff s strike)) spot_levels])
  plot (line ("Put, strike = " <> show strike) [map (\s -> (s, put_payoff s strike)) spot_levels])


main = do
  saveFile "terminal-spot-dist.png" $ do
    terminal_spot_plot
  saveFile "option-payoff.png" $ do
    option_payoff_plot
