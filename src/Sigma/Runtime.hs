module Sigma.Runtime
  ( returnSlot,
    bodyRunner,
    setBodyRunner,
    runBody,
  )
where

import Data.IORef
import Sigma.Lexer (Token)
import Sigma.Types
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE returnSlot #-}
returnSlot :: IORef (Maybe Value)
returnSlot = unsafePerformIO (newIORef Nothing)

{-# NOINLINE bodyRunner #-}
bodyRunner :: IORef (Env -> [Token] -> IO Env)
bodyRunner = unsafePerformIO (newIORef (\env _ -> return env))

setBodyRunner :: (Env -> [Token] -> IO Env) -> IO ()
setBodyRunner = writeIORef bodyRunner

runBody :: Env -> [Token] -> IO Env
runBody env toks = do
  runner <- readIORef bodyRunner
  runner env toks
