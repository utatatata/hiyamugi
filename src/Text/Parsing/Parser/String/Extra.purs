module Text.Parsing.Parser.String.Extra where

import Prelude
import Data.Array (many, some)
import Data.Char.Unicode (toUpper)
import Data.Char.Unicode.Extra (isNewline, isSpaceWithoutNewline)
import Data.Foldable (for_)
import Data.String.CodeUnits (toCharArray)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.String (class StringLike, satisfy)

newline :: forall s m. StringLike s => Monad m => ParserT s m Char
newline = satisfy isNewline

space :: forall s m. StringLike s => Monad m => ParserT s m Char
space = satisfy isSpaceWithoutNewline

spaces :: forall s m. StringLike s => Monad m => ParserT s m Unit
spaces = void $ many space

spaces1 :: forall s m. StringLike s => Monad m => ParserT s m Unit
spaces1 = void $ some space

caseChar :: forall s m. StringLike s => Monad m => Char -> ParserT s m Char
caseChar c = satisfy (\x -> toUpper x == toUpper c)

caseString :: forall s m. StringLike s => Monad m => String -> ParserT s m Unit
caseString s = for_ (toCharArray s) caseChar <?> s
