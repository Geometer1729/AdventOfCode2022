module Problem where
import GHC.Base (Symbol)
import GHC.TypeLits (symbolVal, KnownSymbol)

class
  Show s => Problem (n :: Symbol) p s | n -> p , n -> s where
  parse :: Text -> Maybe p

  solve :: p -> s

run :: forall n p s. KnownSymbol n => Problem n p s => IO ()
run = do
  input <- readFile ("./inputs/" ++ symbolVal (Proxy @n))
  puzle <- case parse @n $ fromString input of
    Nothing -> die "failed to parse"
    Just puzle -> pure puzle
  let sol = solve @n puzle
  print sol
