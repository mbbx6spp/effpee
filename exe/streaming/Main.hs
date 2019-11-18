{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}


module Main where

import           Control.Applicative
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8     as A
import qualified Data.Attoparsec.ByteString.Streaming as A
import qualified Data.ByteString.Streaming.Char8      as Q
import           Data.Char
import           Data.Function                        (($), (&))
import           Data.Int
import           Data.Maybe
import           Data.Text
import           Data.Word
import qualified Effpee.Streaming.JSON                as JSON
import           GHC.Show
import           Streaming
import qualified Streaming.Prelude                    as S
import           System.IO                            (IO)

main :: IO ()
main = void $ JSON.main

main2 :: IO ()
main2
  = Q.getContents
  & A.parsed apacheLogEntry
  & void
  & S.print

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

data IP
  = IPv4 Word8 Word8 Word8 Word8
  deriving (Show)

type ClientId    = Text
type Username    = Text
type Timestamp   = Text
type StatusCode  = Word16
type HTTPRequest = Text
type Bytes       = Int
type Referer     = Text
type UserAgent   = Text

apacheLogEntry
  = pure "ignore for now"
  -- MkLogEntry
  -- <$> parseClientIP
  -- <*> pure Nothing
  -- <*> pure Nothing
  -- <*> parseTimestamp
  -- <*> parseRequest
  -- <*> parseStatus
  -- <*> parseBytes
  -- <*> parseReferer
  -- <*> parseUserAgent

dash = A.char '-'

testIPAddress = many A.decimal <* optional (A.char '.')

