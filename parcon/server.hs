#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package http-conduit

-- Book: Parallel and Concurrent Programming in Haskell
-- Chapter 12: Concurrent Network Servers

import System.IO
import Text.Printf
import Control.Concurrent
import Control.Monad
import Network

talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering
  loop
  where
    loop = do
      line <- hGetLine h
      if line == "end"
        then hPutStrLn h ("Thank you for using the " ++
                         "Haskell doubling service.")
        else do hPutStrLn h (show (2 * (read line :: Integer)))
                loop

main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle) (\_ -> hClose handle)

port :: Int
port = 44444
  
