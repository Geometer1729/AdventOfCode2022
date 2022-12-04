module P4 where

import Flow
import Problem
import Util (tupeBy,read)

type Range = (Int,Int)

instance Problem "4-a" [(Range,Range)] Int where
  parse = lines .> mapM (tupeBy "," $ tupeBy "-" read)
  solve = count oneContains
    where
      oneContains ((a,b),(c,d)) = (a <= c && d <= b) || (c <= a && b <= d)

instance Problem "4-b" [(Range,Range)] Int where
  path = path @"4-a"
  parse = parse @"4-a"
  solve = count (not <. disjoint)
    where
      disjoint ((a,b),(c,d)) = d < a || b < c

count :: (a -> Bool) -> [a] -> Int
count f = filter f .> length
