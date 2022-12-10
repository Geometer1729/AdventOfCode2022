module P10 where

import Problem
import Flow
import Util(read)

import Data.Map qualified as M
import Data.Text qualified as T

instance Problem "10-a" [Ins] (Maybe Int) where
  parse = lines .> mapM parseLn
  solve ops =
    let
      ents = getEnts $ map toInt ops
      signals = sequence [ getVal n ents <&> (* n) | n <- [20,60..220] ]
        in sum <$> signals

instance Problem "10-b" [Ins] Text where
  parse = parse @"10-a"
  showMeth = toString
  solve = map toInt .> getEnts .> render

data Ins = Noop | Addx Int

parseLn :: Text -> Maybe Ins
parseLn = words .> \case
  ["noop"] -> Just Noop
  ["addx",amt] -> Addx <$> read @Int amt
  _ -> Nothing

toInt :: Ins -> (Int,Int)
toInt = \case
          Noop -> (1,0)
          Addx v -> (2,v)

getEnts :: [(Int,Int)] -> Map Int Int
getEnts = scanl ptAdd (1,1) .> M.fromAscList
  where
    ptAdd (a,b) (c,d) = (a+c,b+d)

getVal :: Int -> Map Int Int -> Maybe Int
getVal n m = M.lookup n m <|> M.lookup (n-1) m

render :: Map Int Int -> Text
render m = let
  str = fromString [ getPix m i | i <- [1..240] ]
            in unlines $ T.chunksOf 40 str

getPix :: Map Int Int -> Int -> Char
getPix ents n = case getVal n ents of
                  Nothing -> error $ "getPix failed" <> show n
                  Just reg -> if abs (reg - (n-1) `mod` 40) <= 1
                    then '#'
                    else '.'
