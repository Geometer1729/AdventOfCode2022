module P14 where

import Problem
import Flow
import Util

import Data.Text qualified as T
import Data.Set qualified as S
import Data.List (maximum)

instance Problem "14-a" [[PT]] Int where
  parse = lines .> mapM (T.splitOn " -> " .> mapM (tupeBy "," read))
  solve = inpToSet .> runSand

instance Problem "14-b" [[PT]] Int where
  parse = parse @"14-a"
  solve = inpToSet .> runSandFloor

type PT = (Int,Int)

chainToSet :: [PT] -> Set PT
chainToSet xs = mconcat $ zipWith lineToSet xs (drop 1 xs)
  where
    lineToSet (x1,y1) (x2,y2)
      | x1 == x2 = let yl = min y1 y2; yh = max y1 y2
        in S.fromList [(x1,y)|y<-[yl..yh]]
      | y1 == y2 = let xl = min x1 x2; xh = max x1 x2
        in S.fromList [(x,y1)|x<-[xl..xh]]
      | otherwise = error "bad line"

inpToSet :: [[PT]] -> Set PT
inpToSet = map chainToSet .> mconcat

stepSand :: Set PT -> PT -> Maybe PT
stepSand full (x,y) = find (`S.notMember` full) [(x,y+1),(x-1,y+1),(x+1,y+1)]

dropSand :: Int -> Set PT -> PT -> Maybe PT
dropSand v full pt@(_,y)
    = case stepSand full pt of
        Nothing -> Just pt
        Just pt'
          | y >= v -> Nothing -- sand hit void
          | otherwise -> dropSand v full pt'

runSand :: Set PT -> Int
runSand stone = go (maximum $ snd <$> S.elems stone) stone
  where
    go :: Int -> Set PT -> Int
    go v full =
      case dropSand v full (500,0) of
          Just lands -> 1 + go v (S.insert lands full)
          Nothing -> 0

dropSandFloor :: Int -> Set PT -> PT -> PT
dropSandFloor v full pt@(_,y)
  = case stepSand full pt of
      Nothing -> pt
      Just pt'
        | y == v -> pt'
        | otherwise -> dropSandFloor v full pt'

runSandFloor :: Set PT -> Int
runSandFloor stone = go (maximum $ snd <$> S.elems stone) stone
  where
    go :: Int -> Set PT -> Int
    go v full =
      case dropSandFloor v full (500,0) of
        (500,0) -> 1
        pt -> 1 + go v (S.insert pt full)
