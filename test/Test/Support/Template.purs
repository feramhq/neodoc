-- | Minimal replacement for the `<~>` operator from the removed
-- | `purescript-template-strings` package. Substitutes `${key}` placeholders
-- | in a string with the corresponding (String-valued) record fields.
module Test.Support.Template
  ( substitute
  , (<~>)
  ) where

import Prelude
import Data.FoldableWithIndex (foldlWithIndex)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Foreign.Object (Object)
import Unsafe.Coerce (unsafeCoerce)

substitute :: forall r. String -> Record r -> String
substitute template record =
  foldlWithIndex
    (\key acc val ->
      replaceAll (Pattern ("${" <> key <> "}")) (Replacement val) acc)
    template
    (unsafeCoerce record :: Object String)

infixl 1 substitute as <~>
