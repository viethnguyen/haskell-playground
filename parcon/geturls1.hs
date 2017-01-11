#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package http-conduit

-- Book: Parallel and Concurrent Programming in Haskell
-- Chapter 8: Overlapping Input/Output 

import Control.Concurrent
import Data.ByteString as B
import GetURL

main = do

  -- create two new empty MVars to hold the results 
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar

  -- fork a new thread to download the first URL, when the download is complete, the result is placed in the MVar m1 
  forkIO $ do
    r <- getURL "http://www.wikipedia.org/wiki/Shovel"
    putMVar m1 r

  -- the same for the second URL. 
  forkIO $ do
    r <- getURL "http://www.wikipedia.org/wiki/Spade"
    putMVar m2 r

  -- wait for the results 
  r1 <- takeMVar m1
  r2 <- takeMVar m2

  -- print out 
  print (B.length r1, B.length r2)
    
