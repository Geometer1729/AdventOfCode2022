module P12 where

import Problem
import Flow
import Data.Char (isLower)
import Data.List (maximum)

import Data.Map qualified as M
import Data.Set qualified as S

instance Problem "12-a" [[Pt]] (Maybe Int) where
  parse = lines .> mapM (toString .> mapM fromChar)

  solve l = do
    let m = toM l
    (start,end,es) <- toG m
    pure $ solveRec es end (S.empty,one start)

instance Problem "12-b" [[Pt]] (Maybe Int) where
  parse = lines .> mapM (toString .> mapM fromChar)

  solve l = do
    let m = toM l
    (start,ends,es) <- toG2 m
    pure $ solveRec2 es ends (S.empty,one start)

data Pt
  = S
  | E
  | T Int
  deriving stock (Eq,Ord,Show)

fromChar :: Char -> Maybe Pt
fromChar 'S' = Just S
fromChar 'E' = Just E
fromChar c
  | isLower c = Just $ T $ ord c - ord 'a'
  | otherwise = Nothing


type M = Map (Int,Int) Pt
type G = (Int,Int,Map Int (Set Int))

toM :: [[Pt]] -> M
toM g = M.fromAscList
  [ ((i,j),p) | (i,row) <- zip [0..] g , (j,p) <- zip [0..] row ]

toG :: M -> Maybe G
toG m = let
  ls = M.fromAscList $ zip (M.keys m) [0 :: Int ..]
  (xLen,yLen) = maximum $ M.keys m
  rev = m |> M.toList .> map (\(a,b) -> (b,a)) .> M.fromList
  start = M.lookup S rev >>= (`M.lookup` ls)
  end = M.lookup E rev >>= (`M.lookup` ls)
  cons :: Map Int (Set Int)
  cons = M.fromListWith S.union $ do
    i1 <- [0.. xLen]
    j1 <- [0.. yLen]
    let p1 = (i1,j1)
    Just h1 <- pure $ M.lookup p1 m
    p2 <- [(i1,j1+1),(i1+1,j1)]
    Just h2 <- pure $ M.lookup p2 m
    Just l1 <- pure $ M.lookup p1 ls
    Just l2 <- pure $ M.lookup p2 ls
    [ (l1,one l2) | connects h1 h2 ] ++ [ (l2,one l1) | connects h2 h1 ]
    in (,,) <$> start <*> end <*> pure cons

connects :: Pt -> Pt -> Bool
connects a b = h b <= h a + 1
  where
    h = \case
      S -> 0
      E -> 25
      T n -> n

type S = (Set Int,Set Int)

step :: Map Int (Set Int) -> (Set Int,Set Int) -> (Set Int,Set Int)
step edges (old,new) = let
  new' = foldl' S.union S.empty
    [ es
    | p <- S.toList new
    , Just es <- pure $ M.lookup p edges  ]
    in (S.union old new,S.difference new' old)

solveRec :: Map Int (Set Int) -> Int -> (Set Int,Set Int) -> Int
solveRec es targ (old,new)
  | new == S.empty = error "stuck"
  | S.member targ new = 0
  | otherwise = 1 + solveRec es targ (step es (old,new))

type G2 = (Int,Set Int,Map Int (Set Int))

toG2 :: M -> Maybe G2
toG2 m = let
  ls = M.fromAscList $ zip (M.keys m) [0 :: Int ..]
  (xLen,yLen) = maximum $ M.keys m
  rev = m |> M.toList .> map (\(a,b) -> (b,a)) .> M.fromList
  start = M.lookup E rev >>= (`M.lookup` ls)
  ends = S.fromList
    [ l | (k,h) <- M.toList m
      , h == S || h == T 0
      , Just l <- pure $ M.lookup k ls ]
  cons :: Map Int (Set Int)
  cons = M.fromListWith S.union $ do
    i1 <- [0.. xLen]
    j1 <- [0.. yLen]
    let p1 = (i1,j1)
    Just h1 <- pure $ M.lookup p1 m
    p2 <- [(i1,j1+1),(i1+1,j1)]
    Just h2 <- pure $ M.lookup p2 m
    Just l1 <- pure $ M.lookup p1 ls
    Just l2 <- pure $ M.lookup p2 ls
    [ (l2,one l1) | connects h1 h2 ] ++ [ (l1,one l2) | connects h2 h1 ]
    in (,ends,cons) <$> start

solveRec2 :: Map Int (Set Int) -> Set Int  -> (Set Int,Set Int) -> Int
solveRec2 es ends (old,new)
  | new == S.empty= error "stuck"
  | S.intersection new ends /= S.empty = 0
  | otherwise = 1 + solveRec2 es ends (step es (old,new))
