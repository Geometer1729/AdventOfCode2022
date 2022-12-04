module Util where

import Flow
import Data.Text qualified as T
import Control.Monad(liftM2)

tupeBy :: Text -> (Text -> Maybe a) -> Text -> Maybe (a,a)
tupeBy delim parser = tupeBy' delim parser parser

tupeBy' :: Text -> (Text -> Maybe a) -> (Text -> Maybe a) -> Text -> Maybe (a,a)
tupeBy' delim p1 p2 input =
  T.breakOn delim input
  & bimap p1 (T.drop (T.length delim) .> p2)
  & uncurry (liftM2 (,))

read :: Read a => Text -> Maybe a
read = toString .> readMaybe
