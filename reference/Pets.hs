module Pets where

import Control.Applicative (pure)
import Control.Category    ((<<<))
import Control.Monad       (void, (>>))
import Data.Bool
import Data.Foldable       (foldMap)
import Data.Function       (($))
import Data.Functor        ((<$>), (<&>))
import Data.List           (filter, intersperse)
import Data.Monoid         (Sum (..), mconcat, (<>))
import Data.Text           (Text)
import Data.Traversable    (traverse)
import Data.Int            (Int)
import GHC.Show            (Show (show))
import System.IO           (FilePath, Handle, IO, IOMode (..), hPutStrLn, print , withFile)

data Pet = MkPet { petName :: Text, petAnimal :: Animal } deriving (Show)
data Animal = Cat | Dog deriving (Show)

mkCat name = MkPet name Cat
mkDog name = MkPet name Dog

pets =
  [ mkCat "garfield"
  , mkDog "odie"
  , mkCat "nermal"
  ]

isCat (MkPet _ Cat) = True
isCat _             = False

isDog (MkPet _ Dog) = True
isDog _             = False

cats = filter isCat pets
dogs = filter isDog pets

catNames = cats <&> petName
dogNames = petName <$> dogs

namesString = mconcat <<< intersperse ", "

catNamesString = namesString catNames
dogNamesString = namesString dogNames

writePet :: Handle -> Pet -> IO Pet
writePet h pet@(MkPet name animal) = hPutStrLn h textRep >> pure pet
  where textRep = show animal <> "{ " <> show name <> "}"

writeCats :: FilePath -> [Pet] -> IO ()
writeCats path xs = withFile path WriteMode (\h -> void $ traverse (writePet h) xs)

writeDogs :: FilePath -> [Pet] -> IO ()
writeDogs path xs = withFile path WriteMode (\h -> void $ traverse (writePet h) xs)

scorePet (MkPet _ Cat) = Sum 1
scorePet (MkPet _ Dog) = Sum 2

example :: Sum Int
example = foldMap scorePet pets

main = do
  writeCats "/tmp/cats.txt" cats
  writeDogs "/tmp/dogs.txt" dogs
  print example
