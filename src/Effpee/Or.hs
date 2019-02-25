{-# LANGUAGE TypeOperators #-}

module Effpee.Or where

import Effpee
import Effpee.ADT

type err \/ succ = Or err succ

type Defaultable a = a \/ a
