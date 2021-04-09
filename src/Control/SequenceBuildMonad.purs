-- | Building literal arrays, lists or other types of sequences using the monadic do syntax.
-- | 
-- | #### What does this approach give us over the usual array syntax?  
-- | 
-- | Compare:
-- | 
-- | ```purescript
-- | _test3 = 
-- |   HH.div_ $ sb do
-- |     ae$ HH.text "outer 1"
-- |     ae$ HH.div_ $ sb do
-- |       ae$ HH.text "inner 1"
-- |       ae$ HH.text "inner 2"
-- |     ae$ HH.text "outer 2"
-- | ```
-- | 
-- | with the equivalent traditional form:
-- | 
-- | ```purescript
-- | _test3 = 
-- |   HH.div_ 
-- |     [ HH.text "outer 1"
-- |     , HH.div_ 
-- |       [ HH.text "inner 1"
-- |       , HH.text "inner 2" ]
-- |     , HH.text "outer 2" ]
-- | ```
-- | 
-- | Sequence builder adds a bit of *ergonomy* when editing such sequences, especially reordering and copying and pasting near the start or end 
-- | of the sequence, thanks to the uniform syntax of all elements, avoiding the opening [ and closing ] and the missing comma.  
-- | 
-- | Besides arrays and lists, this sequence builder works for any `Applicative c` with `Monoid (c a)`.
-- | 
-- | Please see the start of the source file for further usage examples.
module Control.SequenceBuildMonad
  (sequenceBuild
  ,sb
  ,addElement
  ,ae
  ,SequenceBuildMonad)
where

import Prelude

-- import Data.Array ((..))
-- import Data.Foldable (fold)
-- import Data.List.Lazy (List)
-- import Data.Maybe (Maybe)
-- import Halogen.HTML as HH

-- _test1A :: Array Int
-- _test1A = 
--   sequenceBuild do
--     ae$ 1
--     ae$ 2
--     ae$ 1+1+1

-- _test1B :: List Int
-- _test1B = 
--   sequenceBuild do
--     ae$ 1
--     ae$ 2
--     ae$ 1+1+1

-- _test1C :: Array (Array Int)
-- _test1C = -- [[1],[2],[3,4,5]]
--   sequenceBuild do
--     ae$ [1]
--     ae$ [2]
--     ae$ 3..5

-- _test1D :: Maybe (Array Int)
-- _test1D = -- Just [1,2,3,4,5]
--   sequenceBuild do
--     ae$ [1]
--     ae$ [2]
--     ae$ 3..5

-- _test1E :: Maybe (Array Int)
-- _test1E = -- Nothing
--   sequenceBuild do
--     pure unit

-- _test2 :: String
-- _test2 = 
--   (fold :: List String -> String) $
--   sequenceBuild do
--     ae$ "start" <> (show 1)
--     ae$ "middle" <> (show 2)
--     ae$ "end"

-- _test3 :: forall a b. HH.HTML a b
-- _test3 = 
--   HH.div_ $ sb do
--     ae$ HH.text "outer 1"
--     ae$ HH.div_ $ sb do
--       ae$ HH.text "inner 1"
--       ae$ HH.text "inner 2"
--     ae$ HH.text "outer 2"

data SequenceBuildMonad c e a = SequenceBuildMonad (c e) a

instance sequenceFunctor :: Functor (SequenceBuildMonad c e) where
  map f (SequenceBuildMonad ce a) = SequenceBuildMonad ce (f a)

instance sequenceApply :: Apply (SequenceBuildMonad c e) where
  apply (SequenceBuildMonad _ f) (SequenceBuildMonad ce a) = SequenceBuildMonad ce (f a)

instance sequenceApplicative :: (Monoid (c e)) => Applicative (SequenceBuildMonad c e) where
  pure e = SequenceBuildMonad mempty e

instance sequenceBind :: (Monoid (c e)) => Bind (SequenceBuildMonad c e) where
  bind (SequenceBuildMonad ce1 e1) f =
    SequenceBuildMonad (ce1 <> ce2) e2
    where
    SequenceBuildMonad ce2 e2 = f e1

instance sequenceMonad :: (Monoid (c e)) => Monad (SequenceBuildMonad c e)

-- | Convert a sequence builder block into a sequence.
sequenceBuild :: forall e c. SequenceBuildMonad c e Unit -> c e
sequenceBuild (SequenceBuildMonad ce _) = ce

-- | A shortcut for `sequenceBuild`
sb :: forall e c. SequenceBuildMonad c e Unit -> c e
sb = sequenceBuild

-- | Add an element to the sequence. This typically appears on each line of the do block.
addElement :: forall e c. (Applicative c) => e -> SequenceBuildMonad c e Unit
addElement e = SequenceBuildMonad (pure e) unit

-- | A shortcut for `addElement`
ae :: forall e c. (Applicative c) => e -> SequenceBuildMonad c e Unit
ae = addElement

