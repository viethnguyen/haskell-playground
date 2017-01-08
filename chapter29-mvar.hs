-- From chapter 29 "Haskell Programming from first principles"

module WhatHappens where

import Control.Concurrent

myData :: IO (MVar Int)
myData = newEmptyMVar

main :: IO ()
main = do
  mv <- myData
  putMVar mv 0
  mv' <- myData
  zero <- takeMVar mv
  print zero 
