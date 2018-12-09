module Island.Diff where

import Control.Lens
import Control.Monad
import Data.Map (Map)
import Data.Monoid
import Data.Text
import Data.Void
import Generics.Eot (HasEot, Eot, fromEot, toEot)
import qualified Data.Map as Map
import qualified Generics.Eot as Eot


-- | The patch @diff x y@ captures the differences between 'x' and 'y', so that they may be re-applied to that same 'x'
-- later on in order to recover 'y'. Potential uses include:
--
-- * More efficient storage, since storing both 'x' and @diff x y@ typically takes less space than storing both 'x' and
--   'y'.
-- * Sending changes more efficiently, since sending a @diff x y@ again typically takes less bandwidth than sending the
--   entire 'y'.
-- * Reviewing and altering the changes before applying them, since a patch is typically a concrete value which can be
--   examined and not an opaque function. For example, the 'Undo' type class can be used to revert part of a patch
--   before applying it. (TODO: implement 'Undo')
-- * Attempting to merge changes instead of using last-writer-wins, since a patch is again a value which can be examined
--   and compared with others to detect conflicts and produce a merged patch. Note that 'mappend' /composes/ patches
--   (one after the other) it does not /merge/ them (both at the same time, unless a merge conflict is detected).
--
-- Patches @diff x y@ and @diff y z@ can be composed using 'mappend', making it possible to obtain a patch 'p' which
-- brings 'x' to 'z' more efficiently than by patching 'x' to 'y' and 'y' to 'z', and then diffing 'x' and 'z'.
--
-- Note that I was careful to say "a patch 'p' which brings 'x' to 'z'", not @diff x z@. The result of applying 'p' and
-- @diff x z@ to 'x' will be 'z' in both cases, but the patch itself might use a different representation. For example,
-- if @diff x y@ detects that file @foo.txt@ has been renamed to @bar.txt@, and @diff y z@ detects that the contents of
-- @bar.txt@ has been deleted, 'p' might say that @foo.txt@'s name and contents have both changed, whereas @diff x z@
-- might say that @foo.txt@ was deleted and that a blank @bar.txt@ has been created.
--
-- It is also possible to apply @diff x y@ to a value other than 'x', in which case the result depends on the details of
-- the patch's representation. For example, if @foo.txt@ doesn't exist, 'p' might do nothing while @diff x z@ might
-- create a blank @bar.txt@.
--
-- Laws:
--
-- > patch (diff x y) x = y
-- > patch mempty = id
-- > patch (p <> q) = patch p >>> patch q
class (Eq a, Eq (Patch a), Monoid (Patch a)) => Diff a where
  type Patch a
  type Patch a = Patch (Eot a)

  diff  :: a -> a -> Patch a
  patch :: Patch a -> a -> a

  default diff :: (HasEot a, Diff (Eot a))
               => a -> a -> Patch (Eot a)
  diff = genericDiff

  default patch :: (HasEot a, Diff (Eot a))
                => Patch (Eot a) -> a -> a
  patch = genericPatch

instance Diff Void where
  type Patch Void = ()

  diff _ _ = ()
  patch () = id

instance Diff () where
  type Patch () = ()

  diff _ _ = ()
  patch () = id


-- * Atomic types

-- $
-- For non-containers, the 'Diff' implementation is boring: a 'Patch' stores the new value, and also the old value so we
-- can go in the other direction. One important design decision is: do we want the update to succeed when applied to a
-- value which differs from the old value we recored? In this implementation, the update fails with a 'Mismatch' error.
-- See the 'Diff' instance for 'Last' if you want an update which always succeeds.
--
-- TODO: implement the 'Diff' instance for 'Last'.
--
-- Use @deriveAtomicDiff ''MyType@ to give derive a 'Diff' instance for a non-container type 'MyType'.
--
-- TODO: implement 'deriveAtomicDiff' using Template Haskell.

atomicDiff :: Eq a => a -> a -> Last a
atomicDiff a1 a2 | a1 == a2  = Last Nothing
                 | otherwise = Last . Just $ a2

atomicPatch :: Last a -> a -> a
atomicPatch (Last (Just a))_ = a
atomicPatch (Last Nothing) a = a


-- | A newtype wrapper which gives an atomic 'Diff' instance to any 'Eq'.
newtype Atomic a = Atomic
  { unAtomic :: a
  } deriving (Show, Eq)

instance Eq a => Diff (Atomic a) where
  type Patch (Atomic a) = Last a

  diff (Atomic a1) (Atomic a2) = atomicDiff a1 a2
  patch p = Atomic . atomicPatch p . unAtomic


instance Diff Bool where
  type Patch Bool = Patch (Atomic Bool)

  diff = atomicDiff
  patch = atomicPatch

instance Diff Int where
  type Patch Int = Patch (Atomic Int)

  diff = atomicDiff
  patch = atomicPatch

instance Diff Text where
  type Patch Text = Patch (Atomic Text)

  diff = atomicDiff
  patch = atomicPatch


-- * Algebraic data types

-- $
-- To 'Diff' your own records and sum types, derive 'Generic' and write an empty 'Diff' instance for your type. This will use a generic 'Patch' representation based on an isomorphic either-of-tuples.
--
-- Alternatively, use @deriveGenericDiff ''MyType@ to give derive the above.
--
-- TODO: implement 'deriveGenericDiff' using Template Haskell.

instance Diff Eot.Void where
  type Patch Eot.Void = ()

  diff _ _ = ()
  patch () = id

genericDiff :: (HasEot a, Diff (Eot a))
            => a -> a -> Patch (Eot a)
genericDiff x y = diff (toEot x) (toEot y)

genericPatch :: (HasEot a, Diff (Eot a))
             => Patch (Eot a) -> a -> a
genericPatch p = fromEot . patch p . toEot


-- Product types

instance (Diff a, Diff b) => Diff (a, b) where
  type Patch (a, b) = (Patch a, Patch b)

  diff (a1, b1) (a2, b2) = (diff a1 a2, diff b1 b2)
  patch (a12, b12) (a1, b1) = (patch a12 a1, patch b12 b1)

_PatchFst :: Monoid b => Review (a, b) a
_PatchFst = unto $ (,mempty)

_PatchSnd :: Monoid a => Review (a, b) b
_PatchSnd = unto $ (mempty,)


-- * Sum types

-- $
-- Sum types are the main reason updates can fail: if we have a 'Patch' which expects a @Left a@ but we 'patch' it to a
-- @Right b@, we have no choice but to fail with an 'UnexpectedRight' error. Another possibility is a 'Patch' which
-- switches from one constructor to another, 'LeftToRight' for example. In this case, the values are so unrelated that
-- we have no choice but to store the entire new value, and also the old value so we can go in the other direction.
-- Since we have an entire replacement value we could use as the answer, we again have the choice to succeed or to fail
-- if we patch such a constructor-swapping 'Patch' to a value which differs from the old value we recored. In this
-- implementation, for consistency with the atomic case, we fail with a 'MismatchedLeft' if that occurs.

data PatchEither a b
  = ReplaceEither (Either a b)
  | PatchEither (Patch a) (Patch b)

_PatchLeft :: Diff b => Review (PatchEither a b) (Patch a)
_PatchLeft = unto $ flip PatchEither mempty

_PatchRight :: Diff a => Review (PatchEither a b) (Patch b)
_PatchRight = unto $ PatchEither mempty

deriving instance (Show a, Show b, Show (Patch a), Show (Patch b)) => Show (PatchEither a b)
deriving instance (Eq   a, Eq   b, Eq   (Patch a), Eq   (Patch b)) => Eq   (PatchEither a b)

instance (Diff a, Diff b) => Monoid (PatchEither a b) where
  mempty = PatchEither mempty mempty
  _                        `mappend` p@ReplaceEither {}  = p
  ReplaceEither (Left  a2) `mappend` PatchEither a23 _   = ReplaceEither . Left  . patch a23 $ a2
  ReplaceEither (Right b2) `mappend` PatchEither _   b23 = ReplaceEither . Right . patch b23 $ b2
  PatchEither a12 b12      `mappend` PatchEither a23 b23 = PatchEither (a12 <> a23) (b12 <> b23)

instance (Diff a, Diff b) => Diff (Either a b) where
  type Patch (Either a b) = PatchEither a b

  diff (Left  a1) (Left  a2) = PatchEither (diff a1 a2) mempty
  diff (Right b1) (Right b2) = PatchEither mempty (diff b1 b2)
  diff _          x          = ReplaceEither x

  patch (ReplaceEither x)   _          = x
  patch (PatchEither a12 _) (Left  a1) = Left  . patch a12 $ a1
  patch (PatchEither _ b12) (Right b1) = Right . patch b12 $ b1


data PatchMaybe a
  = ReplaceMaybe (Maybe a)
  | PatchMaybe (Patch a)

_PatchJust :: Review (PatchMaybe a) (Patch a)
_PatchJust = unto PatchMaybe

deriving instance (Show a, Show (Patch a)) => Show (PatchMaybe a)
deriving instance (Eq   a, Eq   (Patch a)) => Eq   (PatchMaybe a)

instance Diff a => Monoid (PatchMaybe a) where
  mempty = PatchMaybe mempty
  _               `mappend` p@ReplaceMaybe {} = p
  ReplaceMaybe a2 `mappend` PatchMaybe a23    = ReplaceMaybe $ patch a23 <$> a2
  PatchMaybe a12  `mappend` PatchMaybe a23    = PatchMaybe $ a12 <> a23

instance Diff a => Diff (Maybe a) where
  type Patch (Maybe a) = PatchMaybe a

  diff Nothing   Nothing   = mempty
  diff (Just a1) (Just a2) = PatchMaybe (diff a1 a2)
  diff _         x         = ReplaceMaybe x

  patch (ReplaceMaybe x) _ = x
  patch (PatchMaybe p)   x = patch p <$> x


type PatchElement a = Patch (Maybe a)
newtype PatchMap k a = PatchMap
  { unPatchMap :: Map k (PatchElement a)
  }

_PatchMapAt :: (Ord k, Diff a) => k -> Review (PatchMap k a) (PatchElement a)
_PatchMapAt k = unto $ PatchMap . Map.singleton k

deriving instance (Show k, Show a, Show (Patch a)) => Show (PatchMap k a)
deriving instance (Eq   k, Eq   a, Eq   (Patch a)) => Eq   (PatchMap k a)

-- don't store 'mempty' patches, missing keys already mean 'mempty'
nonMEmpty :: (Eq p, Monoid p) => p -> Maybe p
nonMEmpty p = p <$ guard (p /= mempty)

instance (Ord k, Diff a) => Monoid (PatchMap k a) where
  mempty = PatchMap Map.empty
  PatchMap ka12s `mappend` PatchMap ka23s = PatchMap
                                          $ Map.mergeWithKey (\_ -> mappendElements)
                                                             id
                                                             id
                                                             ka12s
                                                             ka23s
    where
      mappendElements :: PatchElement a -> PatchElement a -> Maybe (PatchElement a)
      mappendElements a12 a23 = nonMEmpty $ a12 <> a23

instance (Ord k, Diff a) => Diff (Map k a) where
  type Patch (Map k a) = PatchMap k a

  diff ka1s ka2s = PatchMap
                 $ Map.mergeWithKey (\_ -> diffElements)
                                    (Map.map deleteElement)
                                    (Map.map insertElement)
                                    ka1s
                                    ka2s
    where
      diffElements :: a -> a -> Maybe (PatchElement a)
      diffElements a1 a2 = nonMEmpty $ diff (Just a1) (Just a2)

      deleteElement :: a -> PatchElement a
      deleteElement a1 = diff (Just a1) Nothing

      insertElement :: a -> PatchElement a
      insertElement a2 = diff Nothing (Just a2)

  patch (PatchMap ka12s) ka1s = Map.mergeWithKey (\_ -> patchJust)
                                                 (Map.mapMaybe patchNothing)
                                                 id
                                                 ka12s
                                                 ka1s
    where
      patchJust :: PatchElement a -> a -> Maybe a
      patchJust a12 = patch a12 . Just

      patchNothing :: PatchElement a -> Maybe a
      patchNothing a12 = patch a12 Nothing
