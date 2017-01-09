#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators

-- Book: Parallel and Concurrent Programming in Haskell
-- Chapter 7: Basic Concurrency: Threads and MVars 

import Control.Concurrent
import Control.Monad

data Logger = Logger (MVar LogCommand)
data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l

logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message msg -> do
          putStrLn msg
          loop
        Stop s -> do
          putStrLn "logger: stop"
          putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

main :: IO ()
main = do
  l <- initLogger
  logMessage l "Hello"
  logMessage l "Bye"
  logStop l
