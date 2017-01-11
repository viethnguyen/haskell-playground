#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package http-conduit

-- Book: Parallel and Concurrent Programming in Haskell
-- Chapter 8: Overlapping Input/Output 

import Control.Concurrent
import Control.Monad
import Control.Exception 
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

waitAny :: [Async a] -> IO a
waitAny as = do
  m <- newEmptyMVar
  let forkWait a = forkIO $ do r <- wait a; putMVar m r
  mapM_ forkWait as
  wait (Async m)
  
main = do
  let
    download url = do
      r <- getURL url
      return (url, r)

  as <- mapM (async . download) sites 

  (url, r) <- waitAny as 
  printf "%s was first (%d bytes)\n" url (B.length r)
  mapM_ wait as 
