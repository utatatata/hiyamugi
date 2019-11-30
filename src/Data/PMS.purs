module Hiyamugi.Data.PMS where

import Prelude
import Control.Alt ((<|>))
import Data.Array as A
import Data.Array.Extra as AE
import Data.Char.Unicode.Extra (isNewline)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Text.Parsing.Parser (ParseError, Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (many1Till)
import Text.Parsing.Parser.String (anyChar, eof, string)
import Text.Parsing.Parser.String.Extra (caseString, spaces1)
import Text.Parsing.Parser.Token.Extra (hexInteger, hexIntegerWithLength, integer, integerWithLength)

type PMS
  = { genre :: String
    , title :: String
    , artist :: String
    }

data Command
  = Player PlayStyle
  | Genre String
  | Title String
  | Artist String
  | BPM Int -- default : 130
  | Midifile FileName
  | PlayLevel Int
  | Rank Rank
  | VolWav Int -- parcentage
  | Wav Index FileName -- Index is 01 to FF (Hex)
  | BMP Index FileName -- Index is 01 to FF (Hex), bitmap size must be 256 * 256
  | Random Int
  | If Int
  | EndIf
  | ChannelMessage TrackNumber ChannelNumber Message

type Index
  = Int

type FileName
  = String

type TrackNumber
  = Int -- from 000 to 999

type Message
  = Array Index

data PlayStyle
  = SinglePlay -- 1
  | TwoPlay -- 2
  | DoublePlay -- 3

fromIntPlayStyle :: Int -> Maybe PlayStyle
fromIntPlayStyle 1 = Just SinglePlay

fromIntPlayStyle 2 = Just TwoPlay

fromIntPlayStyle 3 = Just DoublePlay

fromIntPlayStyle _ = Nothing

data Rank
  = VeryHard -- 0
  | Hard -- 1
  | Normal -- 2
  | Easy -- 3

fromIntRank :: Int -> Maybe Rank
fromIntRank 0 = Just VeryHard

fromIntRank 1 = Just Hard

fromIntRank 2 = Just Normal

fromIntRank 3 = Just Easy

fromIntRank _ = Nothing

data ChannelNumber
  = BGM -- 01
  | ChangingATempo -- 03
  | BGA -- 04
  | ChanginPoorBitmap -- 06
  | ObjectChannelOfPlayer1 ObjectChannel -- 11 to 17
  | ObjectChannelOfPlayer2 ObjectChannel -- 21 to 27

fromIntChannelNumber :: Int -> Maybe ChannelNumber
fromIntChannelNumber 1 = Just BGM

fromIntChannelNumber 3 = Just ChangingATempo

fromIntChannelNumber 4 = Just BGA

fromIntChannelNumber 6 = Just ChanginPoorBitmap

fromIntChannelNumber n
  | 11 <= n && n <= 17 = ObjectChannelOfPlayer1 <$> fromIntObjectChannel (n - 10)

fromIntChannelNumber n
  | 21 <= n && n <= 27 = ObjectChannelOfPlayer2 <$> fromIntObjectChannel (n - 20)

fromIntChannelNumber _ = Nothing

data ObjectChannel
  = Button1
  | Button2
  | Button3
  | Button4
  | Button5
  | Button6
  | Button7

fromIntObjectChannel :: Int -> Maybe ObjectChannel
fromIntObjectChannel 1 = Just Button1

fromIntObjectChannel 2 = Just Button2

fromIntObjectChannel 3 = Just Button3

fromIntObjectChannel 4 = Just Button4

fromIntObjectChannel 5 = Just Button5

fromIntObjectChannel 6 = Just Button6

fromIntObjectChannel 7 = Just Button7

fromIntObjectChannel _ = Nothing

parse :: String -> Either ParseError (Array Command)
parse source =
  source
    # toCharArray
    # AE.split isNewline
    # A.filter ((maybe false ((_ == '#') <<< _.head)) <<< A.uncons)
    # traverse (flip runParser command <<< fromCharArray)

command :: Parser String Command
command =
  player
    <|> genre
    <|> title
    <|> artist
    <|> bpm
    <|> midifile
    <|> playLevel
    <|> rank
    <|> volWav
    <|> wav
    <|> bmp
    <|> random
    <|> if_
    <|> endif
    <|> channelMessage
  where
  cmd :: forall a. String -> Parser String a -> Parser String a
  cmd name p = do
    _ <- string "#"
    caseString name
    spaces1
    p

  decimalCmd :: forall a. String -> (Int -> a) -> Parser String a
  decimalCmd name constructor = cmd name (constructor <$> integer)

  hexadecimalCmd :: forall a. String -> (Int -> a) -> Parser String a
  hexadecimalCmd name constructor = cmd name (constructor <$> hexInteger)

  fileCmd :: forall a. String -> Array String -> (String -> a) -> Parser String a
  fileCmd name extensions constructor =
    cmd name do
      s <- fromCharArray <<< A.fromFoldable <$> many1Till anyChar (string ".")
      _ <- foldl (<|>) (fail "one more extensions") $ map string extensions
      pure $ constructor s

  indexedFileCmd :: forall a. String -> Array String -> (Int -> String -> a) -> Parser String a
  indexedFileCmd name extensions constructor = do
    _ <- string "#"
    caseString name
    i <- hexIntegerWithLength 2
    _ <- spaces1
    s <- fromCharArray <<< A.fromFoldable <$> many1Till anyChar (string ".")
    _ <- foldl (<|>) (fail "one more extensions") $ map string extensions
    pure $ constructor i s

  stringCmd :: forall a. String -> (String -> a) -> Parser String a
  stringCmd name constructor =
    cmd name do
      s <- fromCharArray <<< A.fromFoldable <$> many1Till anyChar eof
      pure $ constructor s

  player :: Parser String Command
  player = do
    n <- decimalCmd "player" identity
    fromIntPlayStyle n # maybe (fail $ "#PLAYER " <> show n) (pure <<< Player)

  genre :: Parser String Command
  genre = stringCmd "genre" Genre

  title :: Parser String Command
  title = stringCmd "title" Title

  artist :: Parser String Command
  artist = stringCmd "artist" Artist

  bpm :: Parser String Command
  bpm = decimalCmd "bpm" BPM

  midifile :: Parser String Command
  midifile = fileCmd "midifile" [ "mid", "midi" ] Midifile

  playLevel :: Parser String Command
  playLevel = decimalCmd "playlevel" PlayLevel

  rank :: Parser String Command
  rank = do
    n <- decimalCmd "rank" identity
    fromIntRank n # maybe (fail $ "#RANK " <> show n) (pure <<< Rank)

  volWav :: Parser String Command
  volWav = do
    n <- decimalCmd "volwav" identity
    if n >= 0 && n <= 100 then
      pure $ VolWav n
    else
      fail $ "requires a percentage at '#VolWav " <> show n <> "'"

  wav :: Parser String Command
  wav = indexedFileCmd "wav" [ "wav" ] Wav

  bmp :: Parser String Command
  bmp = indexedFileCmd "bmp" [ "bmp" ] Wav

  random :: Parser String Command
  random = decimalCmd "random" Random

  if_ :: Parser String Command
  if_ = decimalCmd "if" If

  endif :: Parser String Command
  endif = do
    _ <- string "#"
    caseString "endif"
    pure EndIf

  channelMessage :: Parser String Command
  channelMessage = do
    trackNumber <- integerWithLength 3
    channelNumber <- do
      n <- hexIntegerWithLength 2
      fromIntChannelNumber n # maybe (fail $ "requires two hex digit at '" <> show n <> "'") pure
    _ <- string ":"
    message <- A.some $ hexIntegerWithLength 2
    pure $ ChannelMessage trackNumber channelNumber message
