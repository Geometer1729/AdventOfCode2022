{-# LANGUAGE NamedFieldPuns #-}

module P15 where

import Problem
import Flow
import Util

import Data.Text qualified as T
import Data.Set qualified as S

import Data.Char(isDigit)
import Data.List(groupBy)
import Control.Monad.Extra(fold1M)

type Pt = (Int,Int)

instance Problem "15-a" [(Pt,Pt)] Int where
  parse = lines .> mapM
    (\l -> do
    [a,b,c,d] <- nums l
    pure ((a,b),(c,d))
    )
  solve xs =
    let
      row = 2000000
      total = xs |> map (uncurry $ safeInterval row) .> mconcat .> size
      over = overCount row xs
        in total - over

instance Problem "15-b" [(Pt,Pt)] [Int] where
  parse = parse @"15-a"
  solve = findCands .> map (\(x,y) -> x * 4_000_000 + y)
    -- for me this list of guesses is only 3 long

newtype Intervals = Is{getIs :: [(Int,Int)]}
  deriving stock (Eq,Ord,Show)

instance Semigroup Intervals where
  x <> (Is []) = x
  (Is []) <> x = x
  l@(Is ((a,b):xs)) <> r@(Is ((c,d):ys))
    | b < c - 1 = Is $ (a,b):getIs (Is xs <> r)
    | d < a - 1 = Is $ (c,d):getIs (l <> Is ys)
    | otherwise = Is [(min a c,max b d)] <> Is xs <> Is ys

instance Monoid Intervals where
  mempty = Is []

nums :: Text -> Maybe [Int]
nums = T.split (\c -> not $ isDigit c || c == '-') .> filter (/= "") .> mapM read

safeInterval :: Int -> Pt -> Pt -> Intervals
safeInterval row (sx,sy) (bx,by) = let
  d = abs (sx-bx) + abs (sy-by)
  radius = d - abs (sy - row)
  in if radius < 0
        then Is []
        else Is [(sx-radius,sx+radius)]

overCount :: Int -> [(Pt,Pt)] -> Int
overCount row = map snd .> S.fromList .> S.filter (snd .> (== row)) .> length

size :: Intervals -> Int
size (Is xs) = sum [ b-a+1 | (a,b) <- xs]

data Line
    = Up{con :: Int,start :: Int,end :: Int}
    | Dn{con :: Int,start :: Int,end :: Int}
    deriving stock (Eq,Ord,Show,Generic)
    deriving anyclass (NFData)

mkLines :: Pt -> Pt -> [Line]
mkLines (sx,sy) (bx,by) = let
  d = abs (sx-bx) + abs (sy-by) + 1
    in [ Up (sx-sy+d) (sx-d) sx
       , Up (sx-sy-d) sx (sx+d)
       , Dn (sx+sy+d) sx (sx+d)
       , Dn (sx+sy-d) (sx-d) sx
       ]

colinear :: Line -> Line -> Bool
colinear Up{con=l} Up{con=r} = l == r
colinear Dn{con=l} Dn{con=r} = l == r
colinear _ _ = False

combine :: Line -> Line -> Maybe Line
combine
  Up{con,start=startl,end=endl}
  Up{start=startr,end=endr}
  = let
    newStart = min startl startr
    newEnd = max endl endr
     in Just $ Up{con=con,start=newStart,end=newEnd}
combine
  Dn{con,start=startl,end=endl}
  Dn{start=startr,end=endr}
    = let
      newStart = min startl startr
      newEnd = max endl endr
       in Just $ Dn{con=con,start=newStart,end=newEnd}
combine _ _ = Nothing

findCands :: [(Pt,Pt)] -> [Pt]
findCands =
  concatMap (uncurry mkLines)
  .> sort
  .> groupBy colinear
  .> filter (length .> (>= 2))
  .> map (fold1M combine)
   .> catMaybes
   .> \ls -> [ pt | l1 <- ls , l2 <- ls , Just pt <- pure $ intersect l1 l2 ]

intersect :: Line -> Line -> Maybe Pt
intersect
  Up{con=conu,start=startu,end=endu}
  Dn{con=cond,start=startd,end=endd}
    = do
      x <- half $ conu+cond
      y <- half $ cond-conu
      guard $ 0 <= x && x <= 4_000_000
      guard $ 0 <= y && y <= 4_000_000
      guard $ startu <= x && startd <= x
      guard $ x <= endu && x <= endd
      pure (x,y)
intersect _ _ = Nothing
-- intersect gives nothing on Down Up but that's fine
-- because both orders are checked

half :: Int -> Maybe Int
half n = guard (even n) $> n `div` 2
