#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators

-- Book: Parallel and Concurrent Programming in Haskell
-- Chapter 7: Basic Concurrency: Threads and MVars 

import Control.Concurrent hiding (Chan, newChan, writeChan, readChan)
import Control.Monad


type Stream a = MVar (Item a)
data Item a = Item a (Stream a)

data Chan a
  = Chan (MVar (Stream a)) (MVar (Stream a))

newChan :: IO (Chan a)
newChan = do
  hole <- newEmptyMVar
  readVar <- newMVar hole
  writeVar <- newMVar hole
  return (Chan readVar writeVar)

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) val = do
  newHole <- newEmptyMVar
  oldHole <- takeMVar writeVar
  putMVar oldHole (Item val newHole)
  putMVar writeVar newHole 
  
readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
  stream <- takeMVar readVar
  Item val tail <- readMVar stream
  putMVar readVar tail
  return val

dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeVar) = do
  hole <- readMVar writeVar
  newReadVar <- newMVar hole
  return (Chan newReadVar writeVar)


main = do
  c <- newChan
  writeChan c 'a'
  writeChan c 'b'
  writeChan c 'c'
  readChan c >>= print
  readChan c >>= print  
