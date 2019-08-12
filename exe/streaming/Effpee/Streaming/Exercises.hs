module Effpee.Streaming.Exercises where

-- >>> import           Data.Functor
-- >>> import           Streaming
-- >>> import qualified Streaming.Prelude as S
--
-- We can write a transformation and "sink" action:
-- >>> let outPipe = S.print <$> S.take 100
--
-- Download a large CSV file to /path/to/csv:
-- >>> let dataFile = "/path/to/csv"
--
-- Stream the data from the file, taking the first 100 lines and printing it to stdout:
-- >>> S.readFile dataFile outPipe
--
-- Now try counting the number of lines in your CSV file. Note: there is a S.length
-- How would you change outPipe to do this?
--
-- In my case it looked something like this:
-- >>> S.readFile dataFile S.length
-- 810104 :> ()
--
-- Can you use the streaming S.filter to inspect your CSV file?
