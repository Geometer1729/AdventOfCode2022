module P13 where

import Problem
import Flow
import Data.Attoparsec.Text
import Util

import Data.Text qualified as T
import Data.List (elemIndex)

instance Problem "13-a" [(Packet,Packet)] Int where
  parse = T.splitOn "\n\n" .> mapM (tupeBy "\n" parsePacket)
  solve =
    zip [1..]
    .> filter (snd .> uncurry (<=))
    .> map fst
    .> sum

instance Problem "13-b" [Packet] (Maybe Int) where
  parse = lines .> filter (/= "") .> mapM parsePacket
  solve ls = do
    divs@[p1,p2] <- mapM parsePacket ["[[2]]","[[6]]"]
    let sorted = sort $ divs ++ ls
    ind1 <- (+1) <$> elemIndex p1 sorted
    ind2 <- (+1) <$> elemIndex p2 sorted
    pure $ ind1 * ind2

data Packet
  = Int Int
  | L [Packet]
  deriving stock Show

instance Eq Packet where
  (Int a) == (Int b) = a == b
  (Int a) == (L xs) = [Int a] == xs
  (L xs) == (Int a) = xs == [Int a]
  (L xs) == (L ys) = xs == ys

instance Ord Packet where
  compare (Int a) (Int b) = compare a b
  compare (Int a) (L xs) = compare [Int a] xs
  compare (L xs) (Int a) = compare xs [Int a]
  compare (L xs) (L ys) = compare xs ys

parsePacket :: Text -> Maybe Packet
parsePacket = parseOnly parser .> rightToMaybe

parser :: Parser Packet
parser =
  L <$> (string "[" *> sepBy parser (string ",") <* "]")
  <|> (Int <$> decimal)

