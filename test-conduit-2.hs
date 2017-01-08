#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

main :: IO ()
main = do
  putStrLn "List version:"
  print $ take 10 [1..]
  putStrLn ""
  putStrLn "COnduit version:"
  print $ runConduitPure $ yieldMany [1..] .| takeC 10 .| sinkList 
