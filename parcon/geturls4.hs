#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package http-conduit

-- Book: Parallel and Concurrent Programming in Haskell
-- Chapter 8: Overlapping Input/Output 

import Control.Concurrent
import Data.ByteString as B
import Control.Exception 
import GetURL

data Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  forkIO (do r <- try action; putMVar var r) -- the action is wrapped in `try`, which returns `Either SomeException a`
  return (Async var)

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async var) = readMVar var

wait :: Async a -> IO a
wait a = do
  r <- waitCatch a
  case r of
    Left e -> throwIO e
    Right a -> return a
    
main = do
  a1 <- async (getURL "http://www.wikipedia.org/wiki/Shovel")
  a2 <- async (getURL "http://www.wikipedia.org/wiki/Spade")
  r1 <- wait a1
  r2 <- wait a2
  print (B.length r1, B.length r2)
