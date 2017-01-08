#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

magic :: Int -> IO Int
magic x = do
  putStrLn $ "I'm doing magic with " ++ show x
  return $ x * 2
  
main :: IO ()
main = do
  putStrLn "List version:"
  mapM magic (take 10 [1..]) >>= mapM_ print . takeWhile (< 10)
  putStrLn ""
  putStrLn "Conduit version:"
  runConduit
    $ yieldMany [1..]
    .| takeC 10
    .| mapMC magic
    .| takeWhileC (< 18)
    .| mapM_C print
