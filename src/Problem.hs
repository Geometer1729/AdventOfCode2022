module Problem where
import GHC.Base (Symbol)
import GHC.TypeLits (symbolVal, KnownSymbol)

class
  (KnownSymbol n , Show s)
  => Problem (n :: Symbol) p s | n -> p , n -> s where
  path :: String
  path = symbolVal (Proxy @n)

  parse :: Text -> Maybe p

  solve :: p -> s

run :: forall n p s. KnownSymbol n => Problem n p s => IO ()
run = do
  input <- readFile ("./inputs/" ++ path @n)
  puzle <- case parse @n $ fromString input of
    Nothing -> die "failed to parse"
    Just puzle -> pure puzle
  let sol = solve @n puzle
  print sol
