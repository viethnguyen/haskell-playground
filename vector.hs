#!/usr/bin/env stack
-- stack --resolver lts-6.19 --install-ghc runghc --package vector
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)

main :: IO ()
main = do
  lbs <- L.getContents
  -- Create a new 256-size mutable vector
     -- Fill the vector with zeros
  mutable <- M.replicate 256 0
     -- Add all of the bytes from the stdin
  addBytes mutable lbs
     -- Freeze to get an immutable version
  vector <- U.unsafeFreeze mutable
     -- Print the frequency of each byte
     -- In newer vectors: we can use imapM_
  U.zipWithM_ printFreq (U.enumFromTo 0 255) vector

addBytes
  :: (PrimMonad m, M.MVector v Int)
  => v (PrimState m) Int -> L.ByteString -> m ()
addBytes v lbs = mapM_ (addByte v) (L.unpack lbs)

addByte
  :: (PrimMonad m, M.MVector v Int)
  => v (PrimState m) Int -> Word8 -> m ()
addByte v w = do
  oldCount <- M.read v index
     -- Write back the updated count value
  M.write v index (oldCount + 1)
        -- Indices in vectors are alway Ints. Out bytes come in as Word8
  where
    index :: Int
    index = fromIntegral w

printFreq :: Int -> Int -> IO ()
printFreq index count =
  putStrLn $ concat ["Frequency of byte ", show index, ": ", show count]
