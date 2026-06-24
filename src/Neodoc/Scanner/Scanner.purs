module Neodoc.Scanner (
  scan
, Scan (..)
) where

import Prelude
import Data.Bifunctor (lmap)
import Data.List (List(Nil), (:), fromFoldable, catMaybes)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex
import Data.String.Regex (regex, Regex())
import Data.String (length, trim) as String
import Data.String.CodeUnits (fromCharArray) as String
import Data.Array (replicate) as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(Left))
import Neodoc.Unsafe (unsafeFromRight)
import Partial.Unsafe (unsafePartial)
import Data.String.Regex.AnsiRegex (regex) as AnsiRegex
import Neodoc.Scanner.Error

type Scan = {
  usage         :: String
, options       :: List String
, originalUsage :: String
}

scan :: String -> Either ScanError Scan
scan text = lmap ScanError do
  u <- case sections "usage" of
              Nil   -> Left "No usage section found!"
              x:Nil -> pure x
              _     -> Left "Multiple usage sections found!"

  pure {
    usage:         fixSection u
  , options:       fixSection <$> sections "options"
  , originalUsage: String.trim u
  }

  where
    sections n = maybe Nil
                       (catMaybes <<< fromFoldable)
                       (Regex.match (section n) text)

section :: String -> Regex
section name = unsafeFromRight $
  regex ("^([^\n]*" <> name <> "[^\n]*:(?:.*$)\n?(?:(?:[ \t].*)?(?:\n|$))*)")
        (Regex.parseFlags "gmi")

fixSection :: String -> String
fixSection = fixHeaders <<< removeEscapes
  where
    removeEscapes = to (Just ' ') AnsiRegex.regex
    fixHeaders    = to (Just ' ') $ unsafeFromRight
                                  $ regex "(^[^:]+:)" Regex.noFlags
    to c = flip Regex.replace' $ \m _ ->
              maybe "" (replicateChar (String.length m)) c

replicateChar :: Int -> Char -> String
replicateChar n c = String.fromCharArray (Array.replicate n c)


