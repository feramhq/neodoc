-- | Char-based case conversion, replacing the `toLower`/`toUpper` that the
-- | removed `Data.Char.Unicode` (unicode < 6) provided for `Char`. The current
-- | `unicode` package only exposes `CodePoint`-based variants.
module Neodoc.Char
  ( toLower
  , toUpper
  ) where

import Prelude
import Data.Char (fromCharCode)
import Data.CodePoint.Unicode (toLowerSimple, toUpperSimple) as U
import Data.Enum (fromEnum)
import Data.Maybe (fromMaybe)
import Data.String.CodePoints (CodePoint, codePointFromChar)

toLower :: Char -> Char
toLower = convert U.toLowerSimple

toUpper :: Char -> Char
toUpper = convert U.toUpperSimple

convert :: (CodePoint -> CodePoint) -> Char -> Char
convert f c = fromMaybe c (fromCharCode (fromEnum (f (codePointFromChar c))))
