#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators

-- Book: Parallel and Concurrent Programming in Haskell
-- Chapter 7: Basic Concurrency: Threads and MVars 

import Control.Concurrent
import Control.Monad

main = do
  m <- newEmptyMVar
  forkIO $ do putMVar m 'x'; putMVar m 'y'
  r <- takeMVar m
  print r

