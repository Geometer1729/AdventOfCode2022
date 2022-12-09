module P8 where

import Problem
import Flow

import Data.List(maximum)

import Data.Map qualified as M
import Data.Set qualified as S

instance Problem "8-a" ((Int,Int),Map (Int,Int) Int) (Maybe Int) where
  parse w = do
    cs <- sequence $ M.fromList
      [ ((x,y),readMaybe @Int [c]) | (x,l) <- zip [0..] $ lines w , (y,c) <- zip [0..] (toString l) ]
    let p = maximum $ M.keys cs
    pure (p,cs)
  solve (s,m) = length <$> trees s m

instance Problem "8-b" ((Int,Int),Map (Int,Int) Int) Int where
  parse w = do
    cs <- sequence $ M.fromList
      [ ((x,y),readMaybe @Int [c]) | (x,l) <- zip [0..] $ lines w , (y,c) <- zip [0..] (toString l) ]
    let p = maximum $ M.keys cs
    pure (p,cs)
  solve (_s,m) = maximum $ (`senic` m) <$> M.keys m
  -- I think this can be linear time if you look back by
  -- following a chain of trees that block eachother

visible :: [((Int,Int),Int)] -> Set (Int,Int)
visible = visible' (-1)

visible' :: Int -> [((Int,Int),Int)] -> Set (Int,Int)
visible' _ [] = S.empty
visible' m ((p,v):r)
  | m < v = S.insert p $ visible' v r
  | otherwise = visible' m r

views :: (Int,Int) -> Map (Int,Int) Int -> Maybe [[((Int,Int),Int)]]
views (x,y) m = let
  ls = [[ (i,j) | i <- [0..x] ] | j <- [0..y] ]
  rs = [[ (i,j) | i <- reverse [0..x] ] | j <- [0..y] ]
  us = [[ (i,j) | j <- [0..x] ] | i <- [0..x] ]
  ds = [[ (i,j) | j <- reverse [0..y] ] | i <- [0..x] ]
  a = ls ++ rs ++ us ++ ds
   in traverse (traverse (\p -> (p,) <$> M.lookup p m)) a

trees :: (Int,Int) -> Map (Int,Int) Int -> Maybe (Set (Int,Int))
trees p m = foldl' S.union S.empty . map visible <$> views p m

senic :: (Int,Int) -> Map (Int,Int) Int -> Int
senic (x,y) m = let
  th = case M.lookup (x,y) m of
        Just h -> h
        Nothing -> error "point not in map"
  rs = getHeights th m [ (i,y)| i <- [x+1..] ]
  ls = getHeights th m [ (i,y)| i <- [x-1,x-2..] ]
  us = getHeights th m [ (x,j)| j <- [y+1..] ]
  ds = getHeights th m [ (x,j)| j <- [y-1,y-2..] ]
    in length rs * length ls * length us * length ds


getHeights :: Int -> Map (Int,Int) Int -> [(Int,Int)] -> [Int]
getHeights th m =
  map (`M.lookup` m)
  .> takeWhile isJust
  .> catMaybes
  .> break (>= th)
  .> (\(xs,ys) -> xs ++ take 1 ys)
