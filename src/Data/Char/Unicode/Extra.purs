module Data.Char.Unicode.Extra where

import Prelude
import Data.Char (toCharCode)
import Data.Char.Unicode.Internal (uIswspace)

-- | U+000A (Line Feed, \n), U+000B (Line Tabulation), U+000C (Form Feed, \f) or U+000D (Carriage Return, \r)
isNewline :: Char -> Boolean
isNewline c = uc == 0xa || uc == 0xb || uc == 0xc || uc == 0xd
  where
  uc :: Int
  uc = toCharCode c

-- any Unicode space character or U+0009 (Character Tabulation, \r)
isSpaceWithoutNewline :: Char -> Boolean
isSpaceWithoutNewline c =
  if uc <= 0x337 then
    uc == 0x9
  else
    uIswspace uc
  where
  uc :: Int
  uc = toCharCode c
