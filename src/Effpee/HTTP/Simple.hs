module Effpee.HTTP.Simple (app) where

import           Control.Concurrent      (threadDelay)
import           Data.ByteString
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy    as LBS
import           Data.Function           (($))
import           Network.HTTP.Types      (ResponseHeaders, Status (..), status200)
import           Network.Wai
import           System.IO               (IO)

status :: Status
status  = status200

headers :: ResponseHeaders
headers = [("Content-Type", "text/cats")]

body :: ByteString
body    = "A barebones WAI app."

router :: ByteString -> Builder
router "/"        = lazyByteString "This is my new home."
router "/casey"   = lazyByteString "I'm a pirate."
router "/joshua"  = lazyByteString "Missing mom."
router "/spotter" = lazyByteString "I hate cats."
router _          = lazyByteString "Missing."

streamingBody :: ByteString -> (Builder -> IO ()) -> IO () -> IO ()
streamingBody path send flush = do
  send $ router path
  flush
  threadDelay 5000000
  send $ lazyByteString "Bye!"

app :: Application
app req cb =
  cb $ responseStream status headers $ streamingBody $ rawPathInfo req
