module Island.Diff where

import Data.Monoid
import Data.Text
import Data.Void
import Generics.Eot (HasEot, Eot, fromEot, toEot)
import qualified Generics.Eot as Eot


-- | A 'Patch' can transform an 'x' into a 'y', and then (once 'invert'ed) back to the original 'x'.
--
-- Applying a 'Patch' on the 'x' from which it was created using 'diff' will always succeed and will produce the 'y'
-- which was given to 'diff', otherwise it might produce a different value, or it might fail if the given 'x' is
-- 'Incompatible' with the requested change. Implementations which always succeed should set 'Incompatible' to 'Void'.
--
-- 'compose' is useful to convert between incremental backups and differential backups. If you have a full backup of 'x'
-- and you also want to be able to restore to two later points 'y' and 'z', you can either use incremental backups by
-- storing @diff x y@ and @diff y z@, or differential backups by storing @diff x y@ and @diff x z@. You can convert from
-- incremental backups to differential backups by composing your @diff x y@ and your @diff y z@ into a 'Patch'
-- corresponding to @diff x z@. To convert differential backups into incremental backups, remember that each patch can
-- be 'invert'ed, so you also have @diff z x@. Compose it with your @diff x y@ to obtain @diff z y@, and 'invert' it to
-- get @diff y z@.
--
-- Expressed as laws:
--
-- > patch (diff x y) x = y
-- > patch mempty = id
-- > patch (p <> q) = patch p >>> patch q
class Monoid (Patch a) => Diff a where
  type Patch a

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
-- To 'Diff' your own records and sum types, derive 'Generic' and write a mostly-empty 'Diff' instance for your type.
-- "Mostly", because it's not possible to give a default implementation for the associated type, so you'll have to write
-- something like this:
--
-- > instance Diff MyType where
-- >   type Patch MyType        = Patch        (Eot MyType)
-- >   type Incompatible MyType = Incompatible (Eot MyType)
--
-- And then your type will use a generic 'Patch' representation based on an isomorphic either-of-tuples.
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


instance Diff Bool where
  type Patch Bool = Patch (Eot Bool)


-- Product types

instance (Diff a, Diff b) => Diff (a, b) where
  type Patch (a, b) = (Patch a, Patch b)

  diff (a1, b1) (a2, b2) = (diff a1 a2, diff b1 b2)
  patch (a12, b12) (a1, b1) = (patch a12 a1, patch b12 b1)


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
deriving instance (Show a, Show b, Show (Patch a), Show (Patch b)) => Show (PatchEither a b)

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
