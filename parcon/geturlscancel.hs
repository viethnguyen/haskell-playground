#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package http-conduit

-- Book: Parallel and Concurrent Programming in Haskell
-- Chapter 9: Cancellation and Timeout

import Data.Either 
import Control.Concurrent
import Control.Monad
import Control.Exception 
import qualified Data.ByteString as B
import GetURL
import TimeIt
import Text.Printf
import System.IO

data Async a = Async ThreadId (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  m <- newEmptyMVar
  t <- forkIO (do r <- try action; putMVar m r)
  return (Async t m)

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async _ var)  = readMVar var 

cancel :: Async a -> IO ()
cancel (Async  t var) = throwTo t ThreadKilled 

sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.com/wiki/Spade",
         "http://www.wikipedia.com/wiki/Shovel"]

timeDownload :: String -> IO ()
timeDownload url = do
  (page, time) <- timeit $ getURL url
  printf "downloaded %s (%d bytes, %.2fs)\n" url (B.length page) time
  
main = do
  as <- mapM (async . timeDownload) sites
  forkIO $ do
    hSetBuffering stdin NoBuffering
    forever $ do
      c <- getChar
      when (c == 'q') $ mapM_ cancel as

  rs <- mapM waitCatch as
  printf "%d/%d succeeded\n" (length (rights rs)) (length rs)
