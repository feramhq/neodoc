-- | Bidirectional JSON (de)serialization of a `Spec`, used to expose the spec
-- | to JavaScript `transforms` hooks. The layout nesting is represented as
-- | plain arrays (so a hook may e.g. set `spec.layouts = []`), and layout
-- | elements as tagged objects (`{ type, ... }`).
module Neodoc.Spec.Json
  ( usageSpecToJson
  , usageSpecFromJson
  , solvedSpecToJson
  , solvedSpecFromJson
  ) where

import Prelude

import Data.Argonaut.Core (Json, jsonNull)
import Data.Argonaut.Decode (decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Foreign.Object (Object)

import Neodoc.Data.Description (Description(..))
import Neodoc.Data.Layout (Layout(..), Branch)
import Neodoc.Data.OptionArgument (OptionArgument(..))
import Neodoc.Data.SolvedLayout (SolvedLayout, SolvedLayoutArg)
import Neodoc.Data.SolvedLayout as Solved
import Neodoc.Data.UsageLayout (UsageLayout, UsageLayoutArg)
import Neodoc.Data.UsageLayout as Usage
import Neodoc.OptionAlias (OptionAlias)
import Neodoc.Spec (Spec(..), Toplevel)
import Neodoc.Value (Value(..))

--------------------------------------------------------------------------------
-- Public entrypoints
--------------------------------------------------------------------------------

usageSpecToJson :: Spec UsageLayout -> Json
usageSpecToJson = specToJson usageLayoutArgToJson

usageSpecFromJson :: Json -> Either JsonDecodeError (Spec UsageLayout)
usageSpecFromJson = specFromJson usageLayoutArgFromJson

solvedSpecToJson :: Spec SolvedLayout -> Json
solvedSpecToJson = specToJson solvedLayoutArgToJson

solvedSpecFromJson :: Json -> Either JsonDecodeError (Spec SolvedLayout)
solvedSpecFromJson = specFromJson solvedLayoutArgFromJson

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

specToJson :: forall a. (a -> Json) -> Spec (Layout a) -> Json
specToJson encArg (Spec s) =
  encodeJson
    { program: s.program
    , layouts: layoutsToJson encArg s.layouts
    , descriptions: Array.fromFoldable (descriptionToJson <$> s.descriptions)
    , helpText: s.helpText
    , shortHelp: s.shortHelp
    }

specFromJson
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (Spec (Layout a))
specFromJson decArg json = do
  obj :: Object Json <- decodeJson json
  program      <- obj .: "program"
  helpText     <- obj .: "helpText"
  shortHelp    <- obj .: "shortHelp"
  layoutsJson  <- obj .: "layouts"
  descsJson    <- obj .: "descriptions"
  layouts      <- layoutsFromJson decArg layoutsJson
  descriptions <- List.fromFoldable <$> traverse descriptionFromJson (descsJson :: Array Json)
  pure $ Spec { program, layouts, descriptions, helpText, shortHelp }

--------------------------------------------------------------------------------
-- Layouts: NonEmpty List (Toplevel (Layout a))
--   Toplevel x = List (NonEmpty List x); a branch is `Branch a`
--------------------------------------------------------------------------------

layoutsToJson
  :: forall a. (a -> Json) -> NonEmpty List (Toplevel (Layout a)) -> Json
layoutsToJson encArg layouts =
  encodeJson $ Array.fromFoldable $ layouts <#> \toplevel ->
    Array.fromFoldable $ branchToJson encArg <$> toplevel

layoutsFromJson
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (NonEmpty List (Toplevel (Layout a)))
layoutsFromJson decArg json = do
  arr :: Array Json <- decodeJson json
  toplevels <- traverse (toplevelFromJson decArg) arr
  pure case List.fromFoldable toplevels of
    -- an empty `layouts` represents a spec that matches no input, modelled as
    -- a single, empty top-level (which the parser turns into an empty branch).
    Nil    -> Nil :| Nil
    x : xs -> x :| xs

toplevelFromJson
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (Toplevel (Layout a))
toplevelFromJson decArg json = do
  arr :: Array Json <- decodeJson json
  List.fromFoldable <$> traverse (branchFromJson decArg) arr

branchToJson :: forall a. (a -> Json) -> Branch a -> Json
branchToJson encArg branch =
  encodeJson $ Array.fromFoldable $ layoutToJson encArg <$> branch

branchFromJson
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (Branch a)
branchFromJson decArg json = do
  arr :: Array Json <- decodeJson json
  layouts <- traverse (layoutFromJson decArg) arr
  case List.fromFoldable layouts of
    Nil    -> Left (TypeMismatch "non-empty branch")
    x : xs -> Right (x :| xs)

--------------------------------------------------------------------------------
-- Layout a = Group Boolean Boolean (NonEmpty List (Branch a)) | Elem a
--------------------------------------------------------------------------------

layoutToJson :: forall a. (a -> Json) -> Layout a -> Json
layoutToJson encArg (Group optional repeatable branches) =
  encodeJson
    { "type": "Group"
    , optional
    , repeatable
    , branches: Array.fromFoldable $ branchToJson encArg <$> branches
    }
layoutToJson encArg (Elem x) = encArg x

layoutFromJson
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (Layout a)
layoutFromJson decArg json = do
  obj :: Object Json <- decodeJson json
  mType :: Maybe String <- obj .:? "type"
  case mType of
    Just "Group" -> do
      optional      <- obj .: "optional"
      repeatable    <- obj .: "repeatable"
      branchesJson  <- obj .: "branches"
      branches      <- traverse (branchFromJson decArg) (branchesJson :: Array Json)
      case List.fromFoldable branches of
        Nil    -> Left (TypeMismatch "non-empty group")
        x : xs -> Right (Group optional repeatable (x :| xs))
    _ -> Elem <$> decArg json

--------------------------------------------------------------------------------
-- UsageLayoutArg
--------------------------------------------------------------------------------

usageLayoutArgToJson :: UsageLayoutArg -> Json
usageLayoutArgToJson = case _ of
  Usage.Command n r ->
    encodeJson { "type": "Command", name: n, repeatable: r }
  Usage.Positional n r ->
    encodeJson { "type": "Positional", name: n, repeatable: r }
  Usage.Option n mArg r ->
    encodeJson
      { "type": "Option", name: n
      , argument: maybe jsonNull optionArgumentToJson mArg, repeatable: r }
  Usage.OptionStack cs mArg r ->
    encodeJson
      { "type": "OptionStack", chars: fromCharArray (Array.fromFoldable cs)
      , argument: maybe jsonNull optionArgumentToJson mArg, repeatable: r }
  Usage.EOA -> encodeJson { "type": "EOA" }
  Usage.Stdin -> encodeJson { "type": "Stdin" }
  Usage.Reference n -> encodeJson { "type": "Reference", name: n }

usageLayoutArgFromJson :: Json -> Either JsonDecodeError UsageLayoutArg
usageLayoutArgFromJson json = do
  obj :: Object Json <- decodeJson json
  typ :: String <- obj .: "type"
  case typ of
    "Command"    -> Usage.Command <$> obj .: "name" <*> obj .: "repeatable"
    "Positional" -> Usage.Positional <$> obj .: "name" <*> obj .: "repeatable"
    "Option"     -> Usage.Option
                      <$> obj .: "name"
                      <*> optionArgumentFromObj obj
                      <*> obj .: "repeatable"
    "OptionStack" -> do
      chars <- obj .: "chars"
      cs <- charsToNonEmpty chars
      Usage.OptionStack cs <$> optionArgumentFromObj obj <*> obj .: "repeatable"
    "Reference"  -> Usage.Reference <$> obj .: "name"
    "EOA"        -> Right Usage.EOA
    "Stdin"      -> Right Usage.Stdin
    _            -> Left (TypeMismatch ("unknown layout type: " <> typ))

--------------------------------------------------------------------------------
-- SolvedLayoutArg
--------------------------------------------------------------------------------

solvedLayoutArgToJson :: SolvedLayoutArg -> Json
solvedLayoutArgToJson = case _ of
  Solved.Command n r ->
    encodeJson { "type": "Command", name: n, repeatable: r }
  Solved.Positional n r ->
    encodeJson { "type": "Positional", name: n, repeatable: r }
  Solved.Option alias mArg r ->
    encodeJson
      { "type": "Option", name: encodeJson alias
      , argument: maybe jsonNull optionArgumentToJson mArg, repeatable: r }
  Solved.EOA -> encodeJson { "type": "EOA" }
  Solved.Stdin -> encodeJson { "type": "Stdin" }

solvedLayoutArgFromJson :: Json -> Either JsonDecodeError SolvedLayoutArg
solvedLayoutArgFromJson json = do
  obj :: Object Json <- decodeJson json
  typ :: String <- obj .: "type"
  case typ of
    "Command"    -> Solved.Command <$> obj .: "name" <*> obj .: "repeatable"
    "Positional" -> Solved.Positional <$> obj .: "name" <*> obj .: "repeatable"
    "Option"     -> Solved.Option
                      <$> (obj .: "name" >>= decodeJson)
                      <*> optionArgumentFromObj obj
                      <*> obj .: "repeatable"
    "EOA"        -> Right Solved.EOA
    "Stdin"      -> Right Solved.Stdin
    _            -> Left (TypeMismatch ("unknown layout type: " <> typ))

--------------------------------------------------------------------------------
-- OptionArgument, Description, Value
--------------------------------------------------------------------------------

optionArgumentToJson :: OptionArgument -> Json
optionArgumentToJson (OptionArgument name optional) =
  encodeJson { name, optional }

optionArgumentFromObj
  :: Object Json -> Either JsonDecodeError (Maybe OptionArgument)
optionArgumentFromObj obj = do
  mArg :: Maybe Json <- obj .:? "argument"
  case mArg of
    Nothing -> pure Nothing
    Just argJson -> do
      argObj :: Object Json <- decodeJson argJson
      Just <$> (OptionArgument <$> argObj .: "name" <*> argObj .: "optional")

descriptionToJson :: Description -> Json
descriptionToJson (OptionDescription aliases repeatable mArg mDefault mEnv) =
  encodeJson
    { "type": "OptionDescription"
    , aliases: Array.fromFoldable (encodeJson <$> aliases)
    , repeatable
    , argument: maybe jsonNull optionArgumentToJson mArg
    , "default": maybe jsonNull valueToJson mDefault
    , env: maybe jsonNull encodeJson mEnv
    }
descriptionToJson CommandDescription =
  encodeJson { "type": "CommandDescription" }

descriptionFromJson :: Json -> Either JsonDecodeError Description
descriptionFromJson json = do
  obj :: Object Json <- decodeJson json
  typ :: String <- obj .: "type"
  case typ of
    "CommandDescription" -> Right CommandDescription
    "OptionDescription" -> do
      aliasArr :: Array Json <- obj .: "aliases"
      aliases <- traverse decodeJson aliasArr :: Either JsonDecodeError (Array OptionAlias)
      aliases' <- case List.fromFoldable aliases of
        Nil    -> Left (TypeMismatch "non-empty aliases")
        x : xs -> Right (x :| xs)
      repeatable <- obj .: "repeatable"
      mArg <- optionArgumentFromObj obj
      mDefaultJson :: Maybe Json <- obj .:? "default"
      mDefault <- traverse valueFromJson mDefaultJson
      mEnv :: Maybe String <- obj .:? "env"
      pure $ OptionDescription aliases' repeatable mArg mDefault mEnv
    _ -> Left (TypeMismatch ("unknown description type: " <> typ))

valueToJson :: Value -> Json
valueToJson = encodeJson

-- best-effort decode mirroring `encodeJsonValue`
valueFromJson :: Json -> Either JsonDecodeError Value
valueFromJson json =
  orElse (BoolValue <$> decodeJson json) $
  orElse (IntValue <$> decodeJson json) $
  orElse (FloatValue <$> decodeJson json) $
  orElse (StringValue <$> decodeJson json) $
  (ArrayValue <$> (traverse valueFromJson =<< decodeJson json))

orElse :: forall e a. Either e a -> Either e a -> Either e a
orElse (Right a) _ = Right a
orElse (Left _)  b = b

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

charsToNonEmpty :: String -> Either JsonDecodeError (NonEmpty Array Char)
charsToNonEmpty s =
  note (TypeMismatch "non-empty option stack")
    $ (\{ head, tail } -> head :| tail)
    <$> Array.uncons (toCharArray s)
