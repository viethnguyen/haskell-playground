#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package http-conduit

-- Book: Parallel and Concurrent Programming in Haskell
-- Chapter 8: Overlapping Input/Output 

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import GetURL
import TimeIt
import Text.Printf 

data Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  forkIO (do r <- action; putMVar var r)
  return (Async var)

wait :: Async a -> IO a
wait (Async var) = readMVar var

sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.com/wiki/Spade",
         "http://www.wikipedia.com/wiki/Shovel"]

main = do
  m <- newEmptyMVar
  let
    download url = do
      r <- getURL url
      putMVar m (url, r)

  mapM_ (forkIO . download) sites

  (url, r) <- takeMVar m
  printf "%s was first (%d bytes)\n" url (B.length r)
  replicateM_ (length sites - 1) (takeMVar m) 

