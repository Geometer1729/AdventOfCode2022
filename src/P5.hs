module P5 where

import Problem
import Flow
import Util(read)
import Data.Text qualified as T
import Data.Map qualified as M
import Control.Monad (foldM)

instance Problem "5-a" (Pos,[Ins]) (Maybe Text) where
  parse (lines -> ls) = do
    let (pos',drop 1 -> ins') = break T.null ls
    pos <- parsePos pos'
    ins <- mapM parseIns ins'
    pure (pos,ins)

  solve (pos,ins) = foldM (flip $ stepWith T.reverse) pos ins <&> getTops

instance Problem "5-b" (Pos,[Ins]) (Maybe Text) where
  path = path @"5-a"
  parse = parse @"5-a"
  solve (pos,ins) = foldM (flip $ stepWith id) pos ins <&> getTops

type Pos = Map Int Text
type Ins = (Int,Int,Int)

parsePos :: [Text] -> Maybe (Map Int Text)
parsePos (reverse -> ls) = do
  (nums':chunks) <- pure $ T.chunksOf 4 <$> ls
  nums <- mapM (T.strip .> read @Int) nums'
  let strs = map (fromString .> T.strip) $ transpose $ map (map (`T.index` 1)) chunks
  pure $ M.fromAscList $ zip nums strs

parseIns :: Text -> Maybe Ins
parseIns l = do
  ["move",amt',"from",start',"to",end'] <- pure $ words l
  (,,) <$> read amt' <*> read start' <*> read end'

stepWith :: (Text -> Text) -> Ins -> Pos -> Maybe Pos
stepWith f (amt,start,end) pos = do
  startInit <- M.lookup start pos
  let startNew = T.dropEnd amt startInit
  let moved = T.takeEnd amt startInit
  let pos2 = M.insert start startNew pos
  endInit <- M.lookup end pos
  let endNew = endInit <> f moved
  let pos3 = M.insert end endNew pos2
  pure pos3

getTops :: Pos -> Text
getTops = M.elems .> map T.last .> fromString

