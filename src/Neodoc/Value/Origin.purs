module Neodoc.Value.Origin (
    Origin(..)
  , weight
  ) where

import Prelude
import Data.Pretty
import Data.Function (on)
import Data.Generic.Rep
import Data.Generic.Rep.Show (genericShow)

data Origin
  = Argv
  | Environment
  | Default
  | Empty

derive instance eqOrigin :: Eq Origin
derive instance ordOrigin :: Ord Origin
derive instance genericOrigin :: Generic Origin _

instance showOrigin :: Show Origin where
  show = genericShow

instance prettyOrigin :: Pretty Origin where
  pretty Argv = "Argv"
  pretty Environment = "Environment"
  pretty Default = "Default"
  pretty Empty = "Empty"

weight :: Origin -> Int
weight Argv        = 30000
weight Environment = 20000
weight Default     = 10000
weight Empty       = 0
