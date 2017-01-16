#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package async

-- Book: Parallel and Concurrent Programming in Haskell
-- Chapter 11: Higher-Level Concurrency Abstractions

import GetURL
import Control.Concurrent.Async
import qualified Data.ByteString as B

main = do
  (r1,r2) <- concurrently
             (getURL "http://www.wikipedia.org/wiki/Shovel")
             (getURL "http://www.wikipedia.org/wiki/Spade")
  print (B.length r1, B.length r2)
  
             
