{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}



module Effpee.Transforms.Story where

import Data.Functor (Functor (..))
import Data.Int
import Data.List (filter)
import Data.Maybe
import Data.Text (Text, words, dropAround, toLower)
import Data.UUID hiding (toString)
import Effpee
import Effpee.ADT (Boolean)
import GHC.Generics (Generic, Generic1)
import TextShow
import TextShow.Generic

-- Data type definitions

-- Simple data wrappers that have zero runtime overhead but ensure type safety

newtype URL
  = URL Text
  deriving Generic
  deriving (TextShow) via FromGeneric (URL)

newtype StoryId
  = StoryId Int
  deriving Generic
  deriving (TextShow) via FromGeneric (StoryId)

newtype StoryRev
  = StoryRev Int
  deriving Generic
  deriving (TextShow) via FromGeneric (StoryRev)

newtype Bio
  = Bio Text
  deriving Generic
  deriving (TextShow) via FromGeneric (Bio)

newtype Image
  = Image Text
  deriving Generic
  deriving (TextShow) via FromGeneric (Image)

newtype TweetId
  = TweetId Int
  deriving Generic
  deriving (TextShow) via FromGeneric (TweetId)

newtype Candidate
  = Candidate Text
  deriving (Generic)
  deriving (TextShow) via FromGeneric (Candidate)

-- A very crude approximation for a Date, can you think of a better way to define the type?
newtype Date
  = Date (Int, Int, Int)
  deriving (Generic)
  deriving (TextShow) via FromGeneric (Date)

-- | Represents the core structure of a story independent of its content
data Story content
  = MkStory
  { storyId       :: StoryId
  , storyRevision :: StoryRev
  , storyTitle    :: Text
  , storySubtitle :: Maybe Text
  , storyContent  :: content }
  deriving (Generic, Generic1)
  deriving (TextShow) via FromGeneric (Story content)
  deriving (TextShow1) via FromGeneric1 (Story)

-- | Manual definition of the Functor instance for the Story a type
instance Functor Story where
  fmap :: (a -> b) -> Story a -> Story b
  fmap f story = story { storyContent = f (storyContent story) }

data JurisdictionLevel
  = Federal
  | State
  | Local
  deriving (Generic)
  deriving (TextShow) via FromGeneric (JurisdictionLevel)

data Jurisdiction
  = MkJurisdiction JurisdictionLevel Text
  deriving (Generic)
  deriving (TextShow) via FromGeneric (Jurisdiction)

-- >>> election = MkElection (MkJurisdiction Federal "IL-13") [Candidate "Rodney Davis", Candidate "Stephanie Smith"]
data Election
  = MkElection
  { electionPollDate     :: Date
  , electionJurisdiction :: Jurisdiction
  , electionCandidates   :: [Candidate] }
  deriving (Generic)
  deriving (TextShow) via FromGeneric Election

data TopNItem
  = MkTopNItem Image Text
  deriving (Generic)
  deriving (TextShow) via FromGeneric TopNItem

data TweetAnalysis
  = MkTweetAnalysis TweetId Text
  deriving (Generic)
  deriving (TextShow) via FromGeneric TweetAnalysis

-- | A more structured representations of story content for a buzzfeed-style microsite
data StoryContent
  = TopNList { topNIntro :: Text, topNItems :: [TopNItem] }
  | Obituary { obitImage :: Image, obitBio :: Text }
  | Opinion { opinionImage :: Image, opinionBio :: Text, opinionBody :: Text }
  | TweetStorm { tweetStormAnalysis :: [TweetAnalysis] }
  | ElectionReport { electionReportElection :: Election, electionReportUpdates :: [Text] }
  deriving (Generic)
  deriving (TextShow) via FromGeneric StoryContent

-- Simplified representation of HTML we use for our publishing platform
data HTML
  = H1 Text
  | H2 Text
  | Div [HTML]
  | Span Text
  | Para Text
  | Marquee Text
  | IFrame
    { iframeWidth :: Int
    , iframeHeight :: Int
    , iframeSrc :: URL
    , iframeBorder :: Int
    , iframeAllow :: [IFrameCapability]
    , iframeAllowfullscreen :: Boolean }
  deriving (Generic)
  deriving (TextShow) via FromGeneric HTML

data IFrameCapability
  = Accelerometer
  | Autoplay
  | EncryptedMedia
  | Gyroscope
  | PictureInPicture
  deriving (Generic)
  deriving (TextShow) via FromGeneric IFrameCapability

-- Fixture data below for examples

obit :: StoryContent
obit
  = Obituary
  { obitImage = Image "https://www.example/images/me.png"
  , obitBio   = "someone died, I think her name was Spotter!"
  }

obitStory :: Story StoryContent
obitStory
  = MkStory
  { storyId       = StoryId 1234
  , storyRevision = StoryRev 1
  , storyTitle    = "Spotter died!"
  , storySubtitle = Just "Queen of Types"
  , storyContent  = obit
  }

freeformStory :: Story Text
freeformStory
  = MkStory
  { storyId = StoryId 33
  , storyRevision = StoryRev 2
  , storyTitle = "some title"
  , storySubtitle = Nothing
  , storyContent = "<b>This</b> is a freeform story."
  }

-- defaults

defaultEmbedCapabilities
  = [ Accelerometer
    , Autoplay
    , EncryptedMedia
    , Gyroscope
    , PictureInPicture
    ]

-- Transform functions


-- >>> countWords "hello world!\nHow are you doing today?"
-- 7
countWords :: Text -> Int
countWords = length <<< words


-- >>> let marquee = Marquee "This is my beautiful marquee"
-- >>> let html = Div [marquee, H1 "My title", Para "This is my first paragraph."]
-- >>> modernizeHTML html -- strips out marquee
-- Div [Span "This is my beautiful marquee", H1 "My title", Para "This is my first paragraph."]
modernizeHTML :: HTML -> HTML
modernizeHTML (Marquee t) = Span t
modernizeHTML (Div elems) = Div $ (modernizeHTML <$> elems)
modernizeHTML element     = element


-- >>> let textBody = "Donald Trump’s trade war with China is piling up quite a ..."
-- >>> countOcc "Trump" textBody
-- 1
-- >>> countOcc "Carrots" "I love carrots, but hate spinach."
-- 1
-- >>> countOcc "Spinach" "I love carrots, but hate spinach."
-- 1
-- TODO: Exercise - make the following case pass:
-- >>> countOcc "Mary" "Mary's bladder infection caused many logistical problems."
-- 1
-- Note: there is a function called =replace= from the Data.Text module that might be useful.
countOcc :: Text -> Text -> Int
-- countOcc = todo "Effpee.Transforms.Story.countOcc"
countOcc word text = length $ filter (== toLower word) $ words $ toLower $ dropAround punctuation text
   where punctuation c = c == '.' || c == ','


-- TODO: Exercise
-- >>> redact "Trump" textBody
-- Donald T####’s trade war with China is piling up quite a ...
redact :: Text -> Text -> Text
redact = todo "Effpee.Transforms.Story.redact"
-- redact "" = id
-- redact word = _


-- TODO: Exercise
-- >>> expandWidget (URL "https://www.youtube.com/watch?v=rbE53XUtVw0")
-- Just (IFrame 500 315 (URL "https://www.youtube-nocookie.com/embed/rbE5XUtVw0") 0 defaultEmbedCapabilities Yay)
-- >>> expandWidget (URL "https://www.example.com/non-widget-url")
-- Nothing
expandWidget :: URL -> Maybe HTML
expandWidget = todo "Effpee.Transforms.Story.embedWidget"


-- TODO: Exercise
-- >>> htmlize "<b?"
-- Nothing
-- >>> htmlize "<b>Wow</b>"
-- Just (B "Wow")
htmlize :: Text -> Maybe HTML
htmlize = todo "Effpee.Transforms.Story.htmlize"


-- For the sake of easier REPL work

instance TextShow a => Show (Story a) where
  show = toString <<< showb

instance Show HTML where
  show = toString <<< showb

instance Show StoryContent where
  show = toString <<< showb
