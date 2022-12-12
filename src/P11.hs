{-# LANGUAGE TemplateHaskell #-}

module P11 where

import Problem
import Flow
import Util
import Control.Lens hiding ((.>))

import Data.Text qualified as T
import Control.Monad (liftM2)
import Data.Map qualified as M

data Opp
  = Sq
  | Add Integer
  | Mul Integer
  deriving stock (Eq,Ord,Show)

data Monkey
  = Monkey
    { _items :: [Integer]
    , _opp :: Opp
    , _test :: Integer
    , _throws :: (Int,Int)
    }
    deriving stock (Eq,Ord,Show)

$(makeLenses 'Monkey)

parseMonkey :: Text -> Maybe Monkey
parseMonkey = lines .> \case
  [ _head
    , T.strip .> T.stripPrefix "Starting items: " -> Just starts'
    , T.strip .> T.stripPrefix "Operation: new = " -> Just op'
    , T.strip .> T.stripPrefix "Test: divisible by " -> Just test'
    , T.strip .> T.stripPrefix "If true: throw to monkey " -> Just throwTrue
    , T.strip .> T.stripPrefix "If false: throw to monkey " -> Just throwFalse
    ] -> Monkey
          <$> mapM read (T.splitOn ", " starts')
          <*> (case words op' of
                 ["old","+",n] -> Add <$> read n
                 ["old","*","old"] -> Just Sq
                 ["old","*",n] -> Mul <$> read n
                 _ -> Nothing
              )

          <*> read test'
          <*> liftM2 (,) (read throwTrue) (read throwFalse)
  _ -> Nothing

runMonkey :: MM ()
runMonkey = do
  (mn,mm) <- get
  m <- (snd <$>) $ lift $ M.lookup mn mm
  let is = m ^. items
  let is2 = doOp (m ^. opp) .> (`div` 3) <$> is
  let ts = [((if i `mod` m ^. test == 0 then fst else snd) $ m ^. throws,i) | i <- is2 ]
  forM_ ts $ \(target,val) ->
    _2 . at target . _Just . _2 . items  %= (++ [val])
  _2 . at mn . _Just . _2 . items .= []
  _2 . at mn . _Just . _1 += length ts
  _1 %= (+1) .> (`mod` length mm)

roundOf :: MM () -> MM ()
roundOf r = r >> go
    where
      go = do
        c <- fst <$> get
        when (c /= 0) (r >> go)

runMonkey2 :: Integer -> MM ()
runMonkey2 bigMod = do
  (mn,mm) <- get
  m <- (snd <$>) $ lift $ M.lookup mn mm
  let is = m ^. items
  let is2 = doOp (m ^. opp) .> (`mod` bigMod) <$> is
  --let is2 = doOp (m ^. opp) <$> is
  let ts = [((if i `mod` m ^. test == 0 then fst else snd) $ m ^. throws,i) | i <- is2 ]
  forM_ ts $ \(target,val) ->
    _2 . at target . _Just . _2 . items  %= (++ [val])
  _2 . at mn . _Just . _2 . items .= []
  _2 . at mn . _Just . _1 += length ts
  _1 %= (+1) .> (`mod` length mm)

type MM = StateT (Int,Map Int (Int,Monkey)) Maybe

doOp :: Opp -> Integer -> Integer
doOp = \case
  Mul n -> (* n)
  Add n -> (+ n)
  Sq -> \x -> x * x

instance Problem "11-a" [Monkey] (Maybe Int) where
  parse = T.splitOn "\n\n" .> mapM parseMonkey
  solve ms = do
    let start = (0,M.fromList $ zip [0..] (zip (repeat 0) ms))
    end <- execStateT (replicateM_ 20 (roundOf runMonkey)) start
    let cs = sortOn Down $ fst <$> M.elems (snd end)
    pure $ product $ take 2 cs

instance Problem "11-b" [Monkey] (Maybe Int) where
  parse = parse @"11-a"
  solve ms = do
    let start = (0,M.fromList $ zip [0..] (zip (repeat 0) ms))
    let bigMod = product $ (^. test) <$> ms
    end <- execStateT (replicateM_ 10_000 (roundOf $ runMonkey2 bigMod)) start
    let cs = sortOn Down $ fst <$> M.elems (snd end)
    pure $ product $ take 2 cs

