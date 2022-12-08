module P7 where

import Flow
import Problem

import Data.Text qualified as T
import Data.List (groupBy,foldl, minimum)
import Util (read)
import qualified Data.Map as Map
import Data.List.Extra (dropEnd)

instance Problem "7-a" [Command] Int where
  parse input = let
    ls = lines input
    blocks = groupBy (\_ w -> T.head w /= '$') ls
      in mapM parseCom blocks
  solve = foldl (flip step) initS .> known .> size .> fst .> finish

instance Problem "7-b" [Command] Int where
  parse = parse @"7-a"
  solve = foldl (flip step) initS .> known .> size .> fst .> finish2

initS :: S
initS = S{pwd = [] , known = Dir Map.empty}

step :: Command -> S -> S
step (CD Root) s = s{pwd=[]}
step (CD Par) s = s{pwd=dropEnd 1 $ pwd s}
step (CD (Sub n)) s = s{pwd = pwd s ++ [n]}
step (LS es) s = s{known=addEnts es (pwd s) (known s)}

addEnts :: [Ent] -> Path -> Dir -> Dir
addEnts es (p:ps) (Dir d) = Dir $ Map.adjust (addEnts es ps) p d
addEnts es [] (Dir d) = Dir $ Map.union d $ Map.fromList (es <&> \case
  ED name -> (name,Dir Map.empty)
  EF s name -> (name,File s)
                                 )
addEnts _ _ (File _) = error "pwd was a file?"

parseCom :: [Text] -> Maybe Command
parseCom [T.stripPrefix "$ cd "-> p] = CD . readPath <$> p
parseCom ("$ ls":res) = LS <$> mapM parseEnt res
parseCom _ = Nothing

parseEnt :: Text -> Maybe Ent
parseEnt (words -> [info,name]) =
  case info of
    "dir" -> Just $ ED name
    num -> EF <$> read @Int num  <*> pure name
parseEnt _ = Nothing

readPath :: Text -> CDPath
readPath "/" = Root
readPath ".." = Par
readPath w = Sub w

data Command
  = LS [Ent]
  | CD CDPath
  deriving stock (Eq,Ord,Show)

data CDPath
  = Root
  | Par
  | Sub Text
  deriving stock (Eq,Ord,Show)

type Path = [Text]

data Ent
  = ED Text
  | EF Int Text
  deriving stock (Eq,Ord,Show)

data Dir
  = Dir {ents :: Map Text Dir}
  | File Int
  deriving stock (Eq,Ord,Show)

data S
  = S
  { pwd :: Path
  , known :: Dir
  }
  deriving stock (Eq,Ord,Show)

data DirS
  = DirS Int (Map Text DirS)
  | FileS Int
  deriving stock (Eq,Ord,Show)

size :: Dir -> (DirS,Int)
size (File s) = (FileS s,s)
size (Dir es) = let
  sized = Map.map size es
  total = sum $ Map.map snd sized
  es' = Map.map fst sized
    in (DirS total es',total)

finish :: DirS -> Int
finish (FileS _) = 0
finish (DirS s es) = (if s >= 100_000 then 0 else s) + sum (Map.map finish es)

sizes :: DirS -> [Int]
sizes (FileS _) = []
sizes (DirS s es) = s : concatMap sizes es

finish2 :: DirS -> Int
finish2 (FileS _) = error "root is a file?"
finish2 r@(DirS s _) =
  let need = s - 40_000_000
    in minimum $ filter (>= need) (sizes r)
