module Chart where

import System.Environment (getArgs)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

saveFile nm plot = do
  args <- getArgs
  settings <- pure $ case args of
    ["def"] -> def
    [a, b] -> def {_fo_size = (read a, read b)}
    _ -> def {_fo_size = (2000, 1200)}

  toFile settings nm plot
