module Island.Transform where


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
class Transform a where
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
