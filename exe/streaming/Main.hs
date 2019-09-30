module Main (main) where

import           Control.Applicative
import           Data.Attoparsec.ByteString           (anyWord8)
import qualified Data.Attoparsec.ByteString.Char8     as A
import qualified Data.Attoparsec.ByteString.Streaming as A
import qualified Data.ByteString.Streaming.Char8      as Q
import           Data.Char
import           Data.Function                        (($), (&))
import           Data.Int
import           Data.Maybe
import           Data.Text
import           Data.Word
import           GHC.Show
import           Streaming
import qualified Streaming.Prelude                    as S
import           System.IO                            (IO)

main :: IO ()
main
  = Q.getContents
  & A.parsed apacheLogEntry
  & void
  & S.print

data IP
  = IPv4 Word8 Word8 Word8 Word8
  deriving (Show)

data LogEntry
  = MkLogEntry
  { clientIP  :: IP
  , clientId  :: Maybe ClientId
  , username  :: Maybe Username
  , timestamp :: Timestamp
  , request   :: HTTPRequest
  , status    :: StatusCode
  , bytes     :: Bytes
  , referer   :: Referer
  , userAgent :: UserAgent
  } deriving (Show)

type ClientId    = Text
type Username    = Text
type Timestamp   = Text
type StatusCode  = Word16
type HTTPRequest = Text
type Bytes       = Int
type Referer     = Text
type UserAgent   = Text

apacheLogEntry
  = MkLogEntry
  <$> parseClientIP
  <*> pure Nothing
  <*> pure Nothing
  <*> parseTimestamp
  <*> parseRequest
  <*> parseStatus
  <*> parseBytes
  <*> parseReferer
  <*> parseUserAgent


parseClientIP :: A.Parser IP
parseClientIP
  = IPv4
  <$> anyWord8 <* A.char '.'
  <*> anyWord8 <* A.char '.'
  <*> anyWord8 <* A.char '.'
  <*> anyWord8

dash = A.char '-'

parseTimestamp :: A.Parser _
parseTimestamp = _

parseRequest :: A.Parser _
parseRequest = _

parseStatus :: A.Parser _
parseStatus = _

parseBytes :: A.Parser _
parseBytes = _

parseReferer :: A.Parser _
parseReferer = _

parseUserAgent :: A.Parser _
parseUserAgent = _
