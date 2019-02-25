module Effpee.Test
  ( Gen.list
  , Gen.alpha
  , Gen.alphaNum
  , Gen.ascii
  , Gen.unicode
  , Gen.choice
  , Gen.constant
  , Gen.int
  , Gen.int8
  , Gen.int16
  , Gen.int32
  , Gen.int64
  , Gen.word
  , Gen.word8
  , Gen.word16
  , Gen.word32
  , Gen.word64
  , Hedgehog.GenT
  , Range.linear
  -- * Tasty structures
  , TT.testGroup
  , TTH.testProperty
  -- * Hedgehog operators
  , (Hedgehog.===)
  , (Hedgehog./==)
  , Hedgehog.property
  , Hedgehog.forAll
  -- * Tasty TestUnit operators
  , (TTU.@=?)
  , (TTU.@?=)
  , (TTU.@?)
  , TTU.assertEqual
  , TTU.assertFailure
  , TTU.assertBool
  , TTU.testCase
  -- * Function operators
  , ($)
  , (.)
  , const
  , id
  -- * Base types
  , Char
  , Identity
  , Bool (..)
  -- * Applicative operators
  , (<$>)
  , (<*>)
  , pure
  -- * Category operators
  , (>>>)
  , (<<<)
  -- * GHC.Num
  , (+)
  , (*)
  , (-)
  -- * GHC.Real
  , fromIntegral
  , toInteger
  -- * Effpee.ADT types and data constructors
  , Boolean (..)
  , Deferred (..)
  , Many (..)
  , ManyReversed (..)
  , NonEmpty (..)
  , One (..)
  , Option (..)
  , Or (..)
  , Pair (..)
  , USBill (..)
  , USCoin (..)
  -- * Effpee.Stream types and data constructors
  , Stream (..)
  ) where


import           Control.Applicative   (pure, (<$>), (<*>))
import           Control.Category      ((<<<), (>>>))
import           Data.Bool             (Bool (..))
import           Data.Char
import           Data.Function         (const, id, ($), (.))
import           Data.Functor.Identity (Identity)
import           Effpee.ADT
                 ( Boolean (..)
                 , Deferred (..)
                 , Many (..)
                 , ManyReversed (..)
                 , NonEmpty (..)
                 , One (..)
                 , Option (..)
                 , Or (..)
                 , Pair (..)
                 , USBill (..)
                 , USCoin (..)
                 )
import           Effpee.Stream         (Stream (..))
import           GHC.Num               ((*), (+), (-))
import           GHC.Real              (fromIntegral, toInteger)
import qualified Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import qualified Test.Tasty            as TT
import qualified Test.Tasty.Hedgehog   as TTH
import qualified Test.Tasty.HUnit      as TTU

