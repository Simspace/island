module Island.Diff where

import Data.Bifunctor


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
-- > diff x y `apply` x = Right y
-- > invert (diff x y) = diff y x
-- > diff x y `compose` diff y z = Right (diff x z)
class Diff a where
  type Patch        a
  type Incompatible a

  -- | @x -> y -> diff x y@
  diff    :: a -> a -> Patch a

  -- | @diff x y -> diff y x@
  invert  :: Patch a -> Patch a

  -- | @diff x y -> x -> Either (Incompatible x) y@
  apply   :: Patch a -> a -> Either (Incompatible a) a

  -- | @diff x y -> diff y z -> Either (Incompatible y) (diff x z)@
  compose :: Patch a -> Patch a -> Either (Incompatible a) (Patch a)


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

data Replace a = Replace
  { replaceOld :: a
  , replaceNew :: a
  } deriving Show

data Mismatch a = Mismatch
  { mismatchExpected :: a
  , mismatchActual   :: a
  } deriving Show

atomicDiff :: a -> a -> Replace a
atomicDiff = Replace

atomicInvert :: Replace a -> Replace a
atomicInvert (Replace x y) = Replace y x

atomicApply :: Eq a => Replace a -> a -> Either (Mismatch a) a
atomicApply (Replace expectedX y) actualX | actualX == expectedX = Right y
                                          | otherwise            = Left (Mismatch expectedX actualX)

atomicCompose :: Eq a => Replace a -> Replace a -> Either (Mismatch a) (Replace a)
atomicCompose (Replace x actualY) (Replace expectedY z) | actualY == expectedY = Right (Replace x z)
                                                        | otherwise            = Left (Mismatch expectedY actualY)


-- | A newtype wrapper which gives an atomic 'Diff' instance to any 'Eq'.
newtype Atomic a = Atomic
  { unAtomic :: a
  } deriving Show

instance Eq a => Diff (Atomic a) where
  type Patch        (Atomic a) = Replace  a
  type Incompatible (Atomic a) = Mismatch a

  diff (Atomic x) (Atomic y) = atomicDiff x y
  invert = atomicInvert
  apply pAB = fmap Atomic . atomicApply pAB . unAtomic
  compose = atomicCompose


-- Product types

instance (Diff a, Diff b) => Diff (a, b) where
  type Patch        (a, b) = (Patch a, Patch b)
  type Incompatible (a, b) = Either (Incompatible a) (Incompatible b)

  diff (x1, x2) (y1, y2) = (diff x1 y1, diff x2 y2)
  invert (p1, p2) = (invert @a p1, invert @b p2)
  apply (p1, p2) (x1, x2) = (,) <$> first Left  (apply p1 x1)
                                <*> first Right (apply p2 x2)
  compose (pXY1, pXY2) (pYZ1, pYZ2) = (,) <$> first Left  (compose @a pXY1 pYZ1)
                                          <*> first Right (compose @b pXY2 pYZ2)
