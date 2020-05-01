-- Plugin.hs
{-# language ForeignFunctionInterface #-}
module Plugin (init) where

initialise :: IO ()
initialise = do
  putStrLn "Initialising plugin in Haskell."
  putStrLn "... doing some work :)"
  putStrLn "Done initialising plugin in Haskell."

foreign export ccall initialise :: IO ()