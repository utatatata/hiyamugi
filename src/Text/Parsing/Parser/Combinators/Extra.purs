module Text.Parsing.Parser.Combinators.Extra where

import Prelude
import Data.Array (replicate)
import Data.Traversable (sequence)
import Text.Parsing.Parser (ParserT)

count :: forall s m a. Monad m => Int -> ParserT s m a -> ParserT s m (Array a)
count n p
  | n <= 0 = pure []
  | otherwise = sequence (replicate n p)
