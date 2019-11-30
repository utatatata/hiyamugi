module Data.Array.Extra where

import Prelude
import Data.Array (snoc, uncons)
import Data.Maybe (Maybe(..))

split :: forall a. (a -> Boolean) -> Array a -> Array (Array a)
split isDelimiter = flip go { ys: [], yss: [] }
  where
  go :: Array a -> { ys :: Array a, yss :: Array (Array a) } -> Array (Array a)
  go xs { ys, yss } = case uncons xs of
    Nothing -> yss `snoc` ys
    Just { head, tail } ->
      if isDelimiter head then
        go tail { ys: [], yss: yss `snoc` ys }
      else
        go tail { ys: ys `snoc` head, yss }
