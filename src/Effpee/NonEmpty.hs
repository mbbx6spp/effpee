module Effpee.NonEmpty where

import Effpee
import Effpee.ADT

data NonEmpty a = a :- Many a
