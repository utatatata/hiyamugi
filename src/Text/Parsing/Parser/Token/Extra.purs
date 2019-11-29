module Text.Parsing.Parser.Token.Extra where

import Prelude
import Data.Array (some)
import Data.Int (decimal, fromStringAs, hexadecimal)
import Data.Maybe (maybe)
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (ParserT, fail)
import Text.Parsing.Parser.Combinators.Extra (count)
import Text.Parsing.Parser.Token (digit, hexDigit)

integer :: forall m. Monad m => ParserT String m Int
integer = do
  s <- fromCharArray <$> some digit
  fromStringAs decimal s # maybe (fail $ "'" <> s <> "' is not a decimal integer") pure

integerWithLength :: forall m. Monad m => Int -> ParserT String m Int
integerWithLength n = do
  s <- fromCharArray <$> count n digit
  fromStringAs decimal s # maybe (fail $ "'" <> s <> "' is not a decimal integer") pure

hexInteger :: forall m. Monad m => ParserT String m Int
hexInteger = do
  s <- fromCharArray <$> some hexDigit
  fromStringAs hexadecimal s # maybe (fail $ "'" <> s <> "' is not a hexadecimal integer") pure

hexIntegerWithLength :: forall m. Monad m => Int -> ParserT String m Int
hexIntegerWithLength n = do
  s <- fromCharArray <$> count n hexDigit
  fromStringAs hexadecimal s # maybe (fail $ "'" <> s <> "' is not a hexadecimal integer") pure
