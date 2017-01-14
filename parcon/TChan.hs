#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package http-conduit

-- Book: Parallel and Concurrent Programming in Haskell
-- Chapter 10: Software Transactional Memory 

import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar, retry, atomically)

data TChan a = TChan (TVar (TVarList a)) (TVar (TVarList a))

type TVarList a = TVar (TList a)
data TList a = TNil | TCons a (TVarList a)

newTChan :: STM (TChan a)
newTChan = do
  hole <- newTVar TNil
  read <- newTVar hole
  write <- newTVar hole
  return (TChan read write)

readTChan :: TChan a -> STM a
readTChan (TChan readVar _ ) = do
  listHead <- readTVar readVar
  head <- readTVar listHead
  case head of
    TNil -> retry
    TCons val tail -> do
      writeTVar readVar tail
      return val

writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan _ writeVar) a = do
  newListEnd <- newTVar TNil
  listEnd <- readTVar writeVar
  writeTVar writeVar newListEnd
  writeTVar listEnd (TCons a newListEnd)

unGetTChan :: TChan a -> a ->  STM ()
unGetTChan (TChan readVar _) a = do
  listHead <- readTVar readVar
  newHead <- newTVar (TCons a listHead)
  writeTVar readVar newHead

isEmptyTChan :: TChan a -> STM Bool
isEmptyTChan (TChan read _write) = do
  listHead <- readTVar read
  head <- readTVar listHead
  case head of
    TNil -> return True
    TCons _ _ -> return False

main = do
  c <- atomically $ newTChan
  atomically $ writeTChan c 'a'
  atomically (readTChan c) >>= print
  atomically (isEmptyTChan c) >>= print
  atomically $ unGetTChan c 'a'
  atomically (isEmptyTChan c) >>= print
  atomically (readTChan c) >>= print
  
