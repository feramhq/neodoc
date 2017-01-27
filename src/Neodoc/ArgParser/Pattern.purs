module Neodoc.ArgParser.Pattern where

import Prelude
import Debug.Trace
import Data.Function (on)
import Data.Foldable (for_)
import Data.List hiding (many)
import Data.Tuple (Tuple, fst)
import Data.Bifunctor (rmap, lmap)
import Data.Tuple.Nested ((/\), Tuple3)
import Data.Newtype
import Data.Generic
import Data.String as String
import Data.Pretty
import Data.Maybe
import Data.Array as A
import Data.List.Lazy (replicateM)
import Data.List.Lazy as LL
import Data.Either
import Control.Alt
import Control.Monad.Trans.Class (lift)
import Control.Monad.State as State
import Neodoc.Data.Indexed
import Neodoc.Parsing.Parser hiding (error)
import Neodoc.Parsing.Parser.Combinators
import Neodoc.Parsing.Parser.Combinators as Parser
import Neodoc.Parsing.Parser as Parser
import Neodoc.ArgParser.Evaluate (evalParsers')

data Pattern a = LeafPattern   IsOptional IsRepeatable IsFixed a
               | ChoicePattern IsOptional IsRepeatable IsFixed (List (List (Pattern a)))

type IsOptional = Boolean
type IsRepeatable = Boolean
type IsFixed = Boolean
type IsLocked = Boolean
type HasMoved = Boolean
type AllowRepetitions = Boolean
type AllowOmissions = Boolean

derive instance genericPattern :: (Generic a) => Generic (Pattern a)
instance showPattern :: (Generic a, Show a) => Show (Pattern a) where
  show = gShow

instance prettyPattern :: (Pretty a) => Pretty (Pattern a) where
  pretty (LeafPattern o r f a) = "L"
    <>  (if f then "!" else "*")
    <>  (if o then "[" else "(")
    <>  (pretty a)
    <>  (if o then "]" else ")")
    <>  (if r then "..." else "")
    <>  (if f then "!" else "*")
  pretty (ChoicePattern o r f as) = "C"
    <>  (if f then "!" else "*")
    <>  (if o then "[" else "(")
    <>  (intercalate " | " $ ((intercalate " " <<< (pretty <$> _)) <$> _) as)
    <>  (if o then "]" else ")")
    <>  (if r then "..." else "")
    <>  (if f then "!" else "*")

isOptional :: ∀ a. Pattern a -> Boolean
isOptional (LeafPattern o _ _ _) = o
isOptional (ChoicePattern o _ _ _) = o

isRepeatable :: ∀ a. Pattern a -> Boolean
isRepeatable (LeafPattern _ r _ _) = r
isRepeatable (ChoicePattern _ r _ _) = r

isFixed :: ∀ a. Pattern a -> Boolean
isFixed (LeafPattern _ _ f _) = f
isFixed (ChoicePattern _ _ f _) = f

leaf :: ∀ a. a -> Pattern a
leaf = LeafPattern false false false

leafR :: ∀ a. a -> Pattern a
leafR = LeafPattern false true false

leafF :: ∀ a. a -> Pattern a
leafF = LeafPattern false false true

leafRF :: ∀ a. a -> Pattern a
leafRF = LeafPattern false true true

leafO :: ∀ a. a -> Pattern a
leafO = LeafPattern true false false

leafOR :: ∀ a. a -> Pattern a
leafOR = LeafPattern true true false

leafOF :: ∀ a. a -> Pattern a
leafOF = LeafPattern true false true

leafORF :: ∀ a. a -> Pattern a
leafORF = LeafPattern true true true

choiz :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choiz xs = ChoicePattern false false false $ fromFoldable (fromFoldable <$> xs)

choizR :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizR xs = ChoicePattern false true false $ fromFoldable (fromFoldable <$> xs)

choizF :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizF xs = ChoicePattern false false true $ fromFoldable (fromFoldable <$> xs)

choizRF :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizRF xs = ChoicePattern false true true $ fromFoldable (fromFoldable <$> xs)

choizO :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizO xs = ChoicePattern true false false $ fromFoldable (fromFoldable <$> xs)

choizOR :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizOR xs = ChoicePattern true true false $ fromFoldable (fromFoldable <$> xs)

choizOF :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizOF xs = ChoicePattern true false true $ fromFoldable (fromFoldable <$> xs)

choizORF :: ∀ a. Array (Array (Pattern a)) -> Pattern a
choizORF xs = ChoicePattern true true true $ fromFoldable (fromFoldable <$> xs)

type Result a u = Tuple (List a)
                        (Tuple (List (Pattern u))
                                HasMoved)

parse
  :: ∀ i e c s g u a
   . (Generic u, Show u, Show a, Pretty u, Pretty a, Pretty i)
  => (u -> Parser e c s g (List i) a)
  -> List (Pattern u) -- input patterns
  -> Parser e c s g (List i) (List a)
parse f pats = do
  vs <- fst <$> parsePatterns 0 f true Nil pats
  is <- getInput
  case is of
    Nil   -> Parser.return vs
    i : _ -> Parser.fail $ "Unexpected " <> pretty i

indent :: Int -> String
indent l = String.fromCharArray $ LL.toUnfoldable $ LL.take (l * 4) $ LL.repeat ' '

requireMovement p = do
  a /\ b /\ hasMoved <- p
  if not hasMoved
     then Parser.fail "no movement ..."
     else pure $ a /\ b

parsePatterns
  :: ∀ i e c s g u a
   . (Generic u, Show u, Show a, Pretty u, Pretty a, Pretty i)
  => Int
  -> (u -> Parser e c s g (List i) a)
  -> AllowOmissions   -- allow omissions?
  -> List (Pattern u) -- repeatables
  -> List (Pattern u) -- input patterns
  -> Parser e c s g (List i) (Result a u)
parsePatterns l f allowOmit repPats pats =
  let xs = indexed pats
      f' allowOmit' reps pat = do
        vs /\ reps' /\ hasMoved <- case pat of
                LeafPattern _ r _ x -> do
                  vs <- singleton <$> f x
                  pure (vs /\ reps /\ true)
                ChoicePattern _ _ _ xs ->
                  let resetReps = rmap (lmap (const reps))
                   in resetReps <$> do
                        evalParsers'
                          (const 1 {- TODO -})
                          (const 0 {- TODO -})
                          (const Nothing {- TODO -})
                          (\_ _ -> pure unit {- TODO -})
                          -- TODO: do we need to pass `reps` down?
                          (parsePatterns (l + 1) f allowOmit' Nil <$> xs)
        let reps'' = if isRepeatable pat then pat : reps' else reps'
        pure (vs /\ reps'' /\ hasMoved)
   in do
        -- 1. parse the pattern wholly
        vs /\ rep /\ hasMoved <- go f' xs xs Nil false false false false repPats Nil

        -- 2. consume any trailing arguments using the repeating patterns we
        --    learned about so far.
        --    note: parsing the remainder may have consumed input, since we
        --          never substitute values during rep parsing.
        --    note: we ignore any newly learned reps at this point (TODO: is
        --          this correct?)
        vs' <- parseRemainder rep ((fst <$> _) <<< f' false rep)

        pure $ (vs <> vs') /\ rep /\ (hasMoved || (not $ null vs'))
  where
  go
    :: _                  -- the parser function on elements of `u`
    -> _                  -- the original input for given iteration
    -> _                  -- the patterns to match
    -> _                  -- the carry
    -> AllowOmissions     -- allow omissions?
    -> AllowRepetitions   -- allow repetitions?
    -> HasMoved           -- have we consumed any input this far?
    -> IsLocked           -- are we locked from processing any more fixed?
    -> List (Pattern u)   -- repeatable patterns found so far
    -> List a             -- output values
    -> Parser e c s g (List i) (Result a u)
  -- Success!
  go _ _ Nil Nil _ _ hasMoved _ reps out = do
    Parser.return $ out /\ reps /\ hasMoved

  -- Failure: we used repetition and omission to no effect
  go f' _ Nil ((Indexed _ pat):_) tryingOmissions true true _ _ _ | tryingOmissions || not allowOmit = do
    Parser.fail $ "Expected " <> pretty pat

  -- Failure: try again, this time allow repetitions
  go f' orig Nil (_:_) allowOmissions false _ _ reps out = do
    go f' orig orig Nil allowOmissions true false false reps out

  -- Failure: try again, this time start dropping
  go f' _ Nil (carry@(((Indexed _ pat):_))) true true _ _ _ _ = do
    Parser.fail $ "Expected " <> pretty pat

  go f' _ Nil (carry@(((Indexed _ pat):_))) false true _ _ reps out | allowOmit = do
    -- at this point we rotated the entire input and tried to consume via
    -- repetitions, but w/o any luck. it's time for drastic measures by starting
    -- to remove optional elements, ony be one.

    -- TODO: this might need more logic as to which element we throw out first
    --       the current approach is simply removes from left to right.
    let sortedCarry = sortBy (compare `on` getIndex) carry

    case dropFirst (isOptional <<< getIndexedElem) sortedCarry of
      Just carry' -> go f' carry' carry' Nil false false false false reps out <|> do
                        Parser.fail $ "Expected " <> pretty pat
      Nothing -> Parser.fail $ "Expected " <> pretty pat

  go f' _ Nil (carry@(((Indexed _ pat):_))) false true _ _ _ _ = do
    Parser.fail $ "Expected " <> pretty pat

  -- Step: ignore fixed patterns when locked
  go f' orig (x@(Indexed ix pat):xs) carry allowOmissions allowReps hasMoved true reps out | isFixed pat = do
    go f' orig xs (snoc carry x) allowOmissions allowReps hasMoved true reps out

  -- Step: process next element
  go f' orig (x@(Indexed ix pat):xs) carry allowOmissions allowReps hasMoved isLocked reps out = do

    -- 1. try parsing the pattern
    mR <- (Just <$> (Parser.try $ requireMovement (f' allowOmissions reps pat))) <|> (pure Nothing)
    case mR of
      -- if we have a result, and the result consumed input, we're looking
      -- good. we reset the input pattern bar the matched element, clear
      -- the carry and add this pattern to the list of repeated patterns,
      -- (if this pattern allows for repetition). We also reset `allowReps` to
      -- to treat the rest of the pattern as fresh input.
      Just (result /\ reps') -> do
        -- before attempting to parse the next element, we try to eagerly parse
        -- adjacent matches of the same pattern.
        -- note: we do not check `allowReps` because we want adjacent
        --       repetitions to be done eagerly.
        vs' <- if isRepeatable pat
                then parseRemainder (singleton pat) ((fst <$> _) <<< f' false reps)
                else pure Nil
        let orig' = sortBy (compare `on` getIndex) do
                      filter (not <<< (_ == ix) <<< getIndex) orig
        go f' orig' orig' Nil false false true isLocked reps' (out <> result <> vs')

      -- 2. if we did not manage to make a match, we will try to make a match
      --    using all previously matched, repeatable patterns, provided we are
      --    allowed to do so (`allowReps`).
      _ -> do
        mR' <- if allowReps
                  then (Just <$> choice (Parser.try <<< requireMovement
                                                    <<< f' false Nil <$> reps))
                          <|> (pure Nothing)
                  else pure Nothing
        case mR' of
          -- if we have a result, and the result consumed input, we're looking
          -- ok. we reset the input pattern completely, clear the carry and
          -- re-iterate using the new input. Again, we reset `allowReps` to
          -- `false` in order to treat the resulting input as a fresh pattern.
          Just (result' /\ _) ->
            let orig' = sortBy (compare `on` getIndex) do
                          filter (not <<< (_ == ix) <<< getIndex) orig
             in go f' orig' orig' Nil false false true isLocked reps (out <> result')

          -- 3. if, at this point, we still did not manage to make a match, we
          --    rotate this pattern into the carry and give the next pattern a
          --    chance.
          _ -> do
            -- if this pattern is fixed in place we have to make sure not to
            -- place it in front of a previously failed fixed pattern, since
            -- fixed patterns must be parsed in consecutive order.
            go f' orig xs (x:carry) allowOmissions allowReps hasMoved (isFixed pat) reps out

{-
  drop the first matching element from the list and return the
  resulting list or nothing if nothign changed
-}
dropFirst f xs = go xs Nil
  where go Nil _ = Nothing
        go (x:xs) out | f x = Just $ xs <> out
        go (x:xs) out = go xs (x:out)

{-
  Parse remaining elements, applying the given parser to each pattern.
-}
parseRemainder
  :: ∀ i e c s g u a
   . List u -- repeatables
  -> (u -> Parser e c s g (List i) (List a))
  -> Parser e c s g (List i) (List a)
parseRemainder repPats f = do
  go (choice $ f <$> repPats) Nil
  where go p xs = do
          vs <- p <|> pure Nil
          if null vs then pure xs else go p (xs <> vs)
