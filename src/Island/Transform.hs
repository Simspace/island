module Island.Transform where


-- | A 'Patch' can transform an 'a' into a 'b', and then (once 'invert'ed) back to the original 'a'.
--
-- Applying a 'Patch' on the 'a' from which it was created using 'diff' will always succeed and will produce the 'b'
-- which was given to 'diff', otherwise it might produce a different 'b', or it might fail if the given 'a' is
-- 'Incompatible' with the requested change. Implementations which always succeed should set 'Incompatible' to 'Void'.
--
-- 'compose' is useful to convert between incremental backups and differential backups. If you have a full backup of 'a'
-- and you also want to be able to restore to two later points 'b' and to 'c', you can either use incremental backups by
-- storing a @Patch a b@ and a @Patch b c@, or differential backups by storing @Patch a b@ and @Patch a c@. You can
-- convert from incremental backups to differential backups by composing your @Patch a b@ and your @Patch b c@ into a
-- @Patch a c@. To convert differential backups into incremental backups, remember that each patch can be 'invert'ed, so
-- we also have a @Patch c a@. Compose it with your @patch a b@ to obtain a @Patch c b@, and 'invert' it to get a @Patch
-- b c@.
--
-- Expressed as laws:
--
-- > diff a b `apply` a = Right b
-- > invert (diff a b) = diff b a
-- > diff a b `compose` diff b c = Right (diff a c)
class Transform a b where
  type Patch        a b
  type Incompatible a

  diff    :: a -> b -> Patch a b
  invert  :: proxyA a -> proxyB b             -> Patch a b -> Patch b a
  apply   :: proxyB b                         -> Patch a b -> a         -> Either (Incompatible a) b
  compose :: proxyA a -> proxyB b -> proxyC c -> Patch a b -> Patch b c -> Either (Incompatible b) (Patch a c)


-- * Simplified types

-- $
-- While 'Transform' has more precise types, 'Transform'' is more common because applying a 'Patch' to a value typically
-- doesn't change its type. For short, we will use "update" to mean "applying a 'Patch'" from now on.
--
-- Using @Foo'@ for a type synonym which has fewer type parameters than @Foo@ is a naming convention taken from lens.
-- Yes, we're aware that this makes the more common case look less nice than the uncommon case.

type Transform' a = Transform a a
type Patch'     a = Patch     a a

diff' :: Transform' a
      => a -> a -> Patch' a
diff' = diff

invert' :: Transform' a
        => proxy a
        -> Patch' a -> Patch' a
invert' proxy = invert proxy proxy

apply' :: Transform' a
       => proxy a
       -> Patch' a -> a -> Either (Incompatible a) a
apply' = apply

compose' :: Transform' a
         => proxy a
         -> Patch' a -> Patch' a -> Either (Incompatible a) (Patch' a)
compose' proxy = compose proxy proxy proxy
