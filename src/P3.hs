module P3 where

import Flow
import Problem
import Data.Text qualified as T
import Data.List.Extra (chunksOf)
import Data.List (foldl1')
import Data.Char (isLower,isUpper)

instance Problem "3-a" [(Text,Text)] Int where
  parse = lines .> map (\w -> T.splitAt (T.length w `div` 2) w) .> Just
  solve = map (\(a,b) -> value $ T.head $ intersect a b) .> sum

instance Problem "3-b" [[Text]] Int where
  path = path @"3-a"
  parse = lines .> chunksOf 3 .> Just
  solve = map (foldl1' intersect .> T.head .> value) .> sum

value :: Char -> Int
value c
    | isLower c = ord c - ord 'a' + 1
    | isUpper c = ord c - ord 'A' + 27
    | otherwise = error "bad char"

intersect :: Text -> Text -> Text
intersect a b = T.filter (`T.elem` b) a
