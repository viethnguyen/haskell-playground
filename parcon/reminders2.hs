#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc

-- | Parallel and Concurrent Programming
-- | Chapter 7: Basic concurrency: Threads and MVars

import Control.Concurrent
import Text.Printf
import Control.Monad

main = loop
  where
    loop = do 
      s <- getLine -- wait for input from the user
      if s == "exit"
        then return ()
        else do forkIO $ setReminder s
                loop

setReminder :: String -> IO ()
setReminder s = do
  let t = read s :: Int
  printf "Ok, I'll remind you in %d seconds\n" t
  threadDelay (10^6 * t) -- wait for the specified number of seconds 
  printf "%d seconds is up! BING \BEL\n" t  -- when threadDelay returns, the reminder message is printed. 
