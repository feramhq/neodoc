-- | Replacement for the old `unsafePartial <<< fromRight` idiom. As of
-- | `either` v6, `fromRight` is total and takes a default value, so the
-- | partial extraction is expressed via `fromRight'` instead.
module Neodoc.Unsafe
  ( unsafeFromRight
  ) where

import Data.Either (Either, fromRight')
import Partial.Unsafe (unsafeCrashWith)

unsafeFromRight :: forall a b. Either a b -> b
unsafeFromRight = fromRight' (\_ -> unsafeCrashWith "unsafeFromRight: Left")
