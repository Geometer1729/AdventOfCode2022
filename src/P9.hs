module P9 where

import Problem
import Flow
import Util (read)

import Data.Set qualified as S

instance Problem "9-a" [Move] Int where
  parse = lines .> mapM parseMove
  solve ms = let
    steps = toSteps ms
    hpos = scanl ptAdd (0,0) steps
    tpos = nextKnot hpos
      in length $ S.fromList tpos

instance Problem "9-b" [Move] (Maybe Int) where
  parse = parse @"9-a"
  solve ms = let
    steps = toSteps ms
    hpos = scanl ptAdd (0,0) steps
    paths = iterate nextKnot hpos
      in length . S.fromList <$> paths !!? 9

parseMove :: Text -> Maybe Move
parseMove w =
  case words w of
    [dir,dist] ->
      (case dir of
        "L" -> Just L
        "R" -> Just R
        "U" -> Just U
        "D" -> Just D
        _ -> Nothing
      ) <*> read @Int dist
    _ -> Nothing

data Move
  = L Int
  | R Int
  | U Int
  | D Int

toSteps :: [Move] -> [(Int,Int)]
toSteps = concatMap $ \case
  L n -> replicate n (-1,0)
  R n -> replicate n (1,0)
  U n -> replicate n (0,1)
  D n -> replicate n (0,-1)

ptAdd :: (Int,Int) -> (Int,Int) -> (Int,Int)
ptAdd (a,b) (c,d) = (a+c,b+d)

tailStep :: (Int,Int) -> (Int,Int) -> (Int,Int)
tailStep (hx,hy) (tx,ty) =
  case (hx-tx,hy-ty) of
    (0,0) -> (tx,ty)
    (0,dy)
      | dy > 1 -> (tx,ty+1)
      | dy < -1 -> (tx,ty-1)
      | otherwise -> (tx,ty)
    (dx,0)
      | dx > 1 -> (tx+1,ty)
      | dx < -1 -> (tx-1,ty)
      | otherwise -> (tx,ty)
    (dx,dy)
      | dx > 0 && dy > 0 && (dx >  1 || dy >  1) -> (tx+1,ty+1)
      | dx < 0 && dy > 0 && (dx < -1 || dy >  1) -> (tx-1,ty+1)
      | dx > 0 && dy < 0 && (dx >  1 || dy < -1) -> (tx+1,ty-1)
      | dx < 0 && dy < 0 && (dx < -1 || dy < -1) -> (tx-1,ty-1)
      | abs dx <= 1 && abs dy <= 1 -> (tx,ty)
      | otherwise -> error $ "forgot a case: " <> show (dx,dy)

nextKnot :: [(Int,Int)]  -> [(Int,Int)]
nextKnot = scanl' (flip tailStep) (0,0)
