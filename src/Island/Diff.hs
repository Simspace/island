module Island.Diff where

import Control.Monad.Except
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
class Eq a => Diff a where
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

data Replace a b = Replace
  { replaceOld :: a
  , replaceNew :: b
  } deriving Show

data Mismatch a b = Mismatch
  { mismatchExpected :: a
  , mismatchActual   :: b
  } deriving Show

atomicDiff :: a -> b -> Replace a b
atomicDiff = Replace

atomicInvert :: Replace a b -> Replace b a
atomicInvert (Replace x y) = Replace y x

atomicApply :: Eq a => Replace a b -> a -> Either (Mismatch a a) b
atomicApply (Replace expectedX y) actualX | actualX == expectedX = Right y
                                          | otherwise            = Left (Mismatch expectedX actualX)

atomicCompose :: Eq b => Replace a b -> Replace b c -> Either (Mismatch b b) (Replace a c)
atomicCompose (Replace x actualY) (Replace expectedY z) | actualY == expectedY = Right (Replace x z)
                                                        | otherwise            = Left (Mismatch expectedY actualY)


-- | A newtype wrapper which gives an atomic 'Diff' instance to any 'Eq'.
newtype Atomic a = Atomic
  { unAtomic :: a
  } deriving (Show, Eq)

instance Eq a => Diff (Atomic a) where
  type Patch        (Atomic a) = Replace  a a
  type Incompatible (Atomic a) = Mismatch a a

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

-- * Sum types

-- $
-- Sum types are the main reason updates can fail: if we have a 'Patch' which expects a @Left a@ but we 'apply' it to a
-- @Right b@, we have no choice but to fail with an 'UnexpectedRight' error. Another possibility is a 'Patch' which
-- switches from one constructor to another, 'LeftToRight' for example. In this case, the values are so unrelated that
-- we have no choice but to store the entire new value, and also the old value so we can go in the other direction.
-- Since we have an entire replacement value we could use as the answer, we again have the choice to succeed or to fail
-- if we apply such a constructor-swapping 'Patch' to a value which differs from the old value we recored. In this
-- implementation, for consistency with the atomic case, we fail with a 'MismatchedLeft' if that occurs.

data PatchEither a b
  = PatchLeft   (Patch a)
  | PatchRight  (Patch b)
  | LeftToRight (Replace a b)
  | RightToLeft (Replace b a)

data IncompatibleEither a b
  = IncompatibleLeft  (Incompatible a)
  | IncompatibleRight (Incompatible b)
  | UnexpectedLeft
  | UnexpectedRight
  | MismatchedLeft  (Mismatch a a)
  | MismatchedRight (Mismatch b b)

instance (Diff a, Diff b) => Diff (Either a b) where
  type Patch        (Either a b) = PatchEither        a b
  type Incompatible (Either a b) = IncompatibleEither a b

  diff (Left  x) (Left  y) = PatchLeft   (diff x y)
  diff (Left  x) (Right y) = LeftToRight (atomicDiff x y)
  diff (Right x) (Left  y) = RightToLeft (atomicDiff x y)
  diff (Right x) (Right y) = PatchRight  (diff x y)

  invert (PatchLeft  p)  = PatchLeft   (invert @a p)
  invert (PatchRight p)  = PatchRight  (invert @b p)
  invert (LeftToRight p) = RightToLeft (atomicInvert p)
  invert (RightToLeft p) = LeftToRight (atomicInvert p)

  apply (PatchLeft   p) (Left  x) = bimap IncompatibleLeft  Left  (apply p x)
  apply (PatchRight  p) (Right x) = bimap IncompatibleRight Right (apply p x)
  apply (LeftToRight p) (Left  x) = bimap MismatchedLeft    Right (atomicApply p x)
  apply (RightToLeft p) (Right x) = bimap MismatchedRight   Left  (atomicApply p x)
  apply _               (Left  _) = throwError UnexpectedLeft
  apply _               (Right _) = throwError UnexpectedRight

  compose (PatchLeft pXY) (PatchLeft pYZ)
    = bimap IncompatibleLeft PatchLeft (compose @a pXY pYZ)
  compose (PatchLeft pXY) (LeftToRight (Replace y z))
    = do let pYX = invert @a pXY
         x <- first IncompatibleLeft (apply @a pYX y)
         pure $ LeftToRight (Replace x z)
  compose (PatchRight pXY) (PatchRight pYZ)
    = bimap IncompatibleRight PatchRight (compose @b pXY pYZ)
  compose (PatchRight pXY) (RightToLeft (Replace y z))
    = do let pYX = invert @b pXY
         x <- first IncompatibleRight (apply @b pYX y)
         pure $ RightToLeft (Replace x z)
  compose (LeftToRight (Replace x y)) (PatchRight pYZ)
    = do z <- first IncompatibleRight (apply @b pYZ y)
         pure $ LeftToRight (Replace x z)
  compose (LeftToRight (Replace x actualY)) (RightToLeft (Replace expectedY z))
    | actualY == expectedY = pure $ PatchLeft (diff x z)
    | otherwise            = throwError $ MismatchedRight (Mismatch expectedY actualY)
  compose (RightToLeft (Replace x y)) (PatchLeft pYZ)
    = do z <- first IncompatibleLeft (apply @a pYZ y)
         pure $ RightToLeft (Replace x z)
  compose (RightToLeft (Replace x actualY)) (LeftToRight (Replace expectedY z))
    | actualY == expectedY = pure $ PatchRight (diff x z)
    | otherwise            = throwError $ MismatchedLeft (Mismatch expectedY actualY)
  compose (PatchLeft   _) _ = throwError UnexpectedLeft
  compose (PatchRight  _) _ = throwError UnexpectedRight
  compose (LeftToRight _) _ = throwError UnexpectedRight
  compose (RightToLeft _) _ = throwError UnexpectedLeft
