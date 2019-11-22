{-# LANGUAGE NoImplicitPrelude #-}

module Effpee.Alternatives where

import Control.Applicative
import Data.Function       (($), (.))
import Data.Functor
import Data.Maybe
import Data.Text           hiding (empty)
import GHC.Show            (Show (..))

type Login = Text
type Email = Text
type Bio = Text

data User = MkUser Login Email Bio deriving (Show)

-- In a real world application this would be validating if the @Login@ is valid (no curse words, or whatever that means)
getFormLogin :: Applicative f => f Login
getFormLogin = pure "curlyfries"

-- In a real world application this would be validating the format of the email address given in the form
getFormEmail :: Applicative f => f Email
getFormEmail = pure "bla@example.com"

-- In a real world application this would check for spam or something
getFormBio :: Applicative f => f Bio
getFormBio = pure "Here is the bio for this user."

-- Not sure having a default user makes sense in all systems, but it might.
defaultUser :: User
defaultUser = MkUser "admin" "admin@example.com" "some bio"

baseExpr :: Applicative f => f Bio -> f User
baseExpr bio = MkUser <$> getFormLogin <*> getFormEmail <*> bio

-- Produces
-- >>> expr0
-- Just (MkUser "curlyfries" "bla@dailykos.com" "Here is the bio for this user.")
expr0 :: Maybe User
expr0 = baseExpr getFormBio <|> pure defaultUser

-- Produces
-- >>> expr1
-- Just (MkUser "admin" "admin@example.com" "some bio")
expr1 :: Maybe User
expr1 = baseExpr Nothing <|> Just defaultUser

-- Produces
-- >>> expr2
-- Nothing
expr2 :: Maybe User
expr2 = baseExpr Nothing <|> Nothing


-- Another example

data Stage = Staging | Prod | Dev | Test deriving (Show)
newtype Tag = Tag { unComponent :: Text } deriving (Show)
newtype Component = Component { getComponent :: Text } deriving (Show)
data Service = DailyKos | UserMetrics | Infra

instance Show Service where
  show DailyKos    = "dailykos"
  show UserMetrics = "usermetrics"
  show Infra       = "infra"

type Name = Text
type Value = Text
data AwsTag = MkAwsTag Name Value deriving (Show)

generateAwsTagList
  :: Alternative f
  => Service
  -> f Component
  -> Stage
  -> f Tag
  -> [AwsTag]
generateAwsTagList svc mc stg mt
  = mkList <*> mkTagCtx "Service" svcCtx
           <*> mkTagCtx "Component" cmpCtx
           <*> mkTagCtx "Stage" stgCtx
           <*> mkTagCtx "Tag" tagCtx
  where mkTagCtx :: Functor f => Name -> f Value -> f AwsTag
        mkTagCtx n v = MkAwsTag n <$> v
        mkList a b c d = a : b : c : d : []
        toValue :: Show s => s -> Text
        toValue = pack . show
        svcCtx = pure $ toValue svc
        cmpCtx = toValue <$> mc
        stgCtx = pure $ toValue stg
        tagCtx = toValue <$> mt
