#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
{-# LANGUAGE ExtendedDefaultRules #-}
import Conduit

main :: IO ()
main = do
  putStrLn "List version:"
  print $ takeWhile (< 18) $ map (* 2) $ take 10 [1..]
  putStrLn ""
  putStrLn "Conduit version:"
  print $ runConduitPure
    $ yieldMany [1..]
    .| takeC 10
    .| mapC (* 2)
    .| takeWhileC (< 18)
    .| sinkList 
