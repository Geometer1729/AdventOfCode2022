module P2 where

import Flow
import Problem
import Relude.Extra (safeToEnum)

data Move = Rock | Paper | Scisors
  deriving stock (Show,Eq,Ord,Enum,Bounded)

instance Problem "2-a" [(Move,Move)] Int where
  parse = lines .> mapM (
    words .> \case
      [ them' , me' ] -> let
        them = case them' of
                "A" -> Just Rock
                "B" -> Just Paper
                "C" -> Just Scisors
                _ -> Nothing
        me = case me' of
              "X" -> Just Rock
              "Y" -> Just Paper
              "Z" -> Just Scisors
              _ -> Nothing
          in (,) <$> them <*> me
      _ -> Nothing
                        )

  solve = map (uncurry score) .> sum

instance Problem "2-b" [(Move,Move)] Int where
  path = path @"2-a"
  parse = lines .> mapM (
    words .> \case
      [ them' , me' ] -> let
        them = case them' of
                "A" -> Just Rock
                "B" -> Just Paper
                "C" -> Just Scisors
                _ -> Nothing
        me = case me' of
              "X" -> Just 2
              "Y" -> Just 0
              "Z" -> Just 1
              _ -> Nothing
          in (,) <$> them <*> join (liftA2 getRes me them)
      _ -> Nothing
                        )

  solve = solve @"2-a"

getRes :: Int -> Move -> Maybe Move
getRes n m = safeToEnum $ (fromEnum m + n) `mod` 3

score :: Move -> Move -> Int
score them me = let
  resultPoints = (3*) $ (fromEnum me - fromEnum them +1) `mod` 3
  movePoints = fromEnum me + 1
    in resultPoints + movePoints
