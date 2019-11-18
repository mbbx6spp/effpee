module Effpee.Streaming.JSON where

import           Data.ByteString.Streaming       (ByteString, stdin)
import           Data.ByteString.Streaming.Aeson (streamParse)
import           Data.ByteString.Streaming.HTTP
import           Data.Function                   (($), (&))
import           Data.JsonStream.Parser          (Parser, arrayOf, string, (.:))
import           Data.Text                       (Text)
import           Streaming
import qualified Streaming.Prelude               as S
import           System.IO                       (IO)

main = S.print $ streamParse (arrayOf ("fruits" .: string)) input

input :: ByteString IO ()
input = stdin
