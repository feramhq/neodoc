-- | Lenient numeric parsing backed by JavaScript's `parseFloat`/`parseInt`,
-- | replacing the removed `purescript-globals` package.
module Neodoc.Number
  ( readFloat
  , readInt
  , isFinite
  ) where

foreign import readFloat :: String -> Number

-- | Parse a string into a `Number` using the given radix (e.g. `readInt 10`).
foreign import readInt :: Int -> String -> Number

foreign import isFinite :: Number -> Boolean
