{-# LANGUAGE TypeFamilies, FlexibleContexts, RecordWildCards, LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
module Island where
import Data.Monoid
import Data.Maybe

class Monoid (Patch a) => Diff a where
  type Patch a

  patch :: a -> a -> Patch a
  apply :: Patch a -> a -> (a, Patch a)

-- Convience function
applyAll :: (Eq (Patch a), Diff a) => Patch a -> a -> Maybe a
applyAll p x = case apply p x of
  (y, res)
    | res == mempty -> return y
    | otherwise -> Nothing

apply' :: (Eq (Patch a), Diff a) => Patch a -> a -> a
apply' p = fromMaybe (error "apply' was evil") . applyAll p

-- I guess I do need this type?
data EitherPatch a b = EitherPatch
  { replace    :: Maybe (Either a b)
  , leftPatch  :: Patch a
  , rightPatch :: Patch b
  }

deriving instance (Show a, Show b, Show (Patch a), Show (Patch b)) => Show (EitherPatch a b)
deriving instance (Eq a, Eq b, Eq (Patch a), Eq (Patch b)) => Eq (EitherPatch a b)

instance (Diff a, Diff b) => Monoid (EitherPatch a b) where
  mempty = EitherPatch Nothing mempty mempty
  mappend x y = case (replace x, replace y) of
    (a, Nothing) -> EitherPatch
      { replace = a
      , leftPatch  = leftPatch  x <> leftPatch  y
      , rightPatch = rightPatch x <> rightPatch y
      }
    -- Discard prior patches before the replace
    (_, Just a) -> EitherPatch
      { replace = Just a
      , leftPatch  = leftPatch  y
      , rightPatch = rightPatch y
      }

-- Trival instance for Ints
instance Diff Int where
  type Patch Int = Sum Int

  patch x y = Sum $ x - y
  apply (Sum p) x = (x + p, mempty)


instance (Diff a, Diff b) => Diff (Either a b) where
  type Patch (Either a b) = EitherPatch a b
  patch a b = case (a, b) of
    (Left  x, Left  y) -> mempty { leftPatch = patch x y }
    (Right x, Right y) -> mempty { rightPatch = patch x y }
    (      _,       x) -> mempty { replace = Just x }

  apply EitherPatch {..} x =
    -- Pulled this out because I reuse it below
    let eitherApply = \case
          Left l ->
            let (innerApply, innerRes) = apply leftPatch l
            in (Left innerApply, mempty {leftPatch = innerRes, rightPatch = rightPatch})
          Right l ->
            let (innerApply, innerRes) = apply rightPatch l
            in (Right innerApply, mempty {leftPatch = leftPatch, rightPatch = innerRes})

    in case (replace, x) of
      (Just new, _)  -> eitherApply new
      (Nothing, old) -> eitherApply old

testValue0 :: Either Int (Either (Either Int Int) Int)
testValue0 = Right $ Left $ Right 1

testValue1 :: Either Int (Either (Either Int Int) Int)
testValue1 = Right $ Left $ Left 2

testValue2 :: Either Int (Either (Either Int Int) Int)
testValue2 = Right $ Left $ Right 2

testPatch0 :: EitherPatch Int (Either (Either Int Int) Int)
testPatch0 = patch testValue0 testValue1

testPatch1 :: EitherPatch Int (Either (Either Int Int) Int)
testPatch1 = patch testValue0 testValue2

testPatchRes :: EitherPatch Int (Either (Either Int Int) Int)
testPatchRes = testPatch0 <> testPatch1

-- Quick checks of the laws
-- Monoid laws for EitherPatch
monoidAssocCheck :: Bool
monoidAssocCheck
  =  (testPatch0 <> testPatch1) <> testPatchRes
  == testPatch0  <> (testPatch1 <> testPatchRes)

monoidMemptyCheck :: Bool
monoidMemptyCheck
  =  (testPatchRes <> mempty == testPatchRes)
  && (mempty <> testPatchRes == testPatchRes)

-- Diff laws check
patchApplyCheck :: Bool
patchApplyCheck = apply' (patch testValue0 testValue1) testValue0 == testValue1

applyDistributes :: Bool
applyDistributes
  =  (apply testPatch0 . apply' testPatch1) testValue0
  == apply (testPatch1 <> testPatch0) testValue0

