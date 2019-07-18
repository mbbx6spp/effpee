module Main (main) where

import Network.Wai.Handler.Warp (run)
import Effpee.HTTP.Simple (app)
import System.IO (IO)

main :: IO ()
main = run 8080 app
