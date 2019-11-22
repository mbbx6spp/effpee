module Effpee.BindVersusLetVersusWhere
  ( main
  , main2
  , main3
  , main4
  , config
  ) where

import Data.Map  (Map, fromList, lookup)
import Prelude   hiding (lookup)
import System.IO

main :: IO ()
main = do
  userInput <- getLine
  putStrLn userInput

config :: Map String String
config = fromList [("greeting", "Hello Stranger!")]

main2 :: IO ()
main2 = do
  let Just greeting = lookup "greeting" config
  putStrLn greeting

main3 :: IO ()
main3 = getLine >>= putStrLn

main4 :: IO ()
main4 = action (lookup "greeting" config)
  where
   action (Just greeting) = putStrLn greeting
   action Nothing         = getLine >>= putStrLn
