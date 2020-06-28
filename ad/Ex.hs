{-# language NamedFieldPuns #-}
{-# language DeriveGeneric #-}
{-# language OverloadedLabels #-}
module Ex where

import GHC.Generics
import Data.Generics.Labels

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Number.Erf

import Chart

import Forward

ins :: [Double]
ins = [0, 0.01 .. 3*pi]

(sinx, dsinx) = unzip $ table1 sin ins

sin_dsin_plot = do
  layout_title .= "sin and Dsin"
  layout_all_font_styles . font_size %= (2 *)
  plot (line "sin" [zip ins sinx])
  plot (line "Dsin" [zip ins dsinx])

main = do
  saveFile "sin-dsin.png" $ do
    sin_dsin_plot
