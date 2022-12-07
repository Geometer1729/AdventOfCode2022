module P6 where

import Problem
import Flow
import Data.Text qualified as T
import Data.Set qualified as S


instance Problem "6-a" Text SignalState where
  parse = Just
  solve = T.foldl' (step 4) (Left ("",0))

instance Problem "6-b" Text SignalState where
  parse = parse @"6-a"
  solve = T.foldl' (step 14) (Left ("",0))

type SignalState = Either (Text,Int) Int

step :: Int -> SignalState -> Char -> SignalState
step _ (Right n) _ = Right n
step interval (Left (buf,ind)) c
  | length (buf |> toString .> S.fromList) == interval = Right ind
  | otherwise = Left (T.cons c (T.take (interval -1) buf),ind+1)
