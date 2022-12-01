{-# OPTIONS_GHC -Wno-orphans #-}
module P1 where

import Flow
import Data.List.Extra(linesBy, maximum)
import Problem

instance Problem "1-a" [[Int]] Int where
  parse = lines .> linesBy (== "") .> mapM (mapM $ readMaybe @Int <. toString)
  solve = map sum .> maximum

instance Problem "1-b" [[Int]] Int where
  parse = parse @"1-a"
  solve = map sum .> sortOn Down .> take 3 .> sum
