{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Control.Concurrent
import Foreign.Ptr
import System.Exit
import System.IO
import Foreign.StablePtr

type CB = IO (Ptr ())

makeCB :: IO () -> CB
makeCB cb = catch act handle
  where
    handle :: SomeException -> IO (Ptr ())
    handle e = do
      ptr <- newStablePtr e
      pure (castStablePtrToPtr ptr)
    act = do
      cb
      pure nullPtr

foreign import ccall u :: FunPtr CB -> IO (Ptr ())

call :: IO (Ptr ()) -> IO ()
call ffi = do
  res <- ffi
  if res == nullPtr
    then pure ()
    else do
      putStrLn "ffi exception"
      exc :: SomeException <- deRefStablePtr (castPtrToStablePtr res)
      throwIO exc

main = do
  fbad <- funptr (makeCB bad)
  fgood <- funptr (makeCB good)
  call (u fgood)
  call (u fbad)

foreign import ccall "wrapper" funptr :: IO (Ptr ()) -> IO (FunPtr (IO (Ptr ())))

good :: IO ()
good = do
  putStrLn "OK"

bad :: IO ()
bad = do
  ioError (userError "hej")

