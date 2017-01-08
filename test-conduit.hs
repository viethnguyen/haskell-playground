#!/usr/bin/env stack
-- stack --resolver lts-6.19 runghc --package conduit-combinators
import Conduit

main = do
  -- Pure operations: summing numbers
  print $ runConduitPure $ yieldMany [1..10] .| sumC

  -- Exception safe file access: copy a file.
  writeFile "input.txt" "This is a test." -- create the source file
  runConduitRes $ sourceFileBS "input.txt" .| sinkFile "output.txt" -- actual copying
  readFile "output.txt" >>= putStrLn -- prove that it worked

  -- Perform transformation
  print $ runConduitPure $ yieldMany [1..10] .| mapC (+ 1) .| sinkList 
