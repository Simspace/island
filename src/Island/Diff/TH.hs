{-# OPTIONS -Wno-unused-top-binds #-}
module Island.Diff.TH (makeStructuredPatch) where

import Control.Applicative
import Control.Lens.Internal.FieldTH
import Control.Lens.Internal.PrismTH
import Control.Lens.TH
import Control.Monad
import Control.Monad.Trans.Writer
import Data.Char
import Data.Foldable
import Data.List.Extra (wordsBy)
import Data.Monoid
import Data.Traversable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Printf
import TH.ReifySimple

import Island.Diff


-- We would like to generate something like
--
-- > data T
-- > makeLenses ''T
--
-- Since @[q|data T|]@ and @makeLenses ''T@ both have type @Q [Dec]@, it seems like we can simply concatenate the
-- resulting declaration lists, but unfortunately that doesn't work. The problem is that in order to decide which list
-- of declarations it should output, 'makeLenses' tries to lookup the type @T@ in 'Q''s state, but it's not there
-- because @[d|data T|]@ doesn't add @T@ to 'Q''s state. Instead, it returns the list of declarations which, once
-- spliced into the program, will add @T@ to the state of the next splice. This isn't very compositional.
--
-- We can avoid the issue by skipping the name lookup and working directly with the 'Info' it would have returned.
makeLensesForInfo :: Info -> DecsQ
makeLensesForInfo info = case info of
  TyConI dec -> makeFieldOpticsForDec lensRules dec
  _          -> fail "makeLensesForInfo: Expected type constructor name"

makePrismsForInfo :: Info -> DecsQ
makePrismsForInfo info = case info of
  TyConI dec -> makeDecPrisms True dec
  _          -> fail "makePrisms: expected type constructor name"


qualifiedName :: Name -> String
qualifiedName = pprint

unqualifiedName :: Name -> String
unqualifiedName = last . wordsBy (== '.') . qualifiedName

toUppercase :: String -> String
toUppercase []       = []
toUppercase ('_':xs) = toUppercase xs  -- in case it's a lensy name
toUppercase (c:xs)   = toUpper c : xs

prefixedName :: String -> Name -> Name
prefixedName prefix = mkName . (prefix ++) . toUppercase . unqualifiedName


defaultBang :: Q Bang
defaultBang = bang noSourceUnpackedness noSourceStrictness


class TopLevel a where
  declare :: a -> DecsQ

instance TopLevel () where
  declare () = pure []

instance TopLevel Dec where
  declare dec = pure [dec]

instance TopLevel a => TopLevel [a] where
  declare = fmap mconcat . mapM declare

instance TopLevel a => TopLevel (Q a) where
  declare mx = declare =<< mx


class IsType a where
  asType :: a -> TypeQ


class HasFieldTypes a where
  fieldTypes :: a -> [TypeQ]

instance HasFieldTypes () where
  fieldTypes () = []

instance HasFieldTypes a => HasFieldTypes [a] where
  fieldTypes = concatMap fieldTypes


class Patchisize a where
  patchisize :: a -> a

instance Patchisize Type where
  patchisize = AppT (ConT ''Patch)

instance Patchisize a => Patchisize (Q a) where
  patchisize = fmap patchisize


data TypeCon = TypeCon
  { typeConName :: Name
  , typeConTvs  :: [Name]  -- ^ type variables
  }
  deriving (Eq, Show)

instance IsType TypeCon where
  asType (TypeCon {..}) = ctorT typeConName
                        $ fmap varT typeConTvs

instance Patchisize TypeCon where
  patchisize (TypeCon {..})
    = TypeCon (prefixedName "Patch" typeConName)
              typeConTvs


data AnonymousField = AnonymousField
  { anonymousFieldType :: Q Type
  }

dataConFieldToAnonymousField :: (Maybe Name, Q Type) -> Maybe AnonymousField
dataConFieldToAnonymousField (nameMay, type_) = do
  guard (nameMay == Nothing)
  pure $ AnonymousField type_

instance HasFieldTypes AnonymousField where
  fieldTypes (AnonymousField {..}) = [anonymousFieldType]

instance Patchisize AnonymousField where
  patchisize (AnonymousField {..}) = AnonymousField (patchisize anonymousFieldType)

anonymousFieldToBangType :: AnonymousField -> Q BangType
anonymousFieldToBangType (AnonymousField {..})
  = bangType defaultBang anonymousFieldType


data NamedField = NamedField
  { namedFieldName :: Name
  , namedFieldType :: Q Type
  }

dataConFieldToNamedField :: (Maybe Name, Q Type) -> Maybe NamedField
dataConFieldToNamedField (nameMay, type_) = do
  name <- nameMay
  pure $ NamedField name type_

instance HasFieldTypes NamedField where
  fieldTypes (NamedField {..}) = [namedFieldType]

instance Patchisize NamedField where
  patchisize (NamedField {..}) = NamedField (prefixedName "_patch" namedFieldName)
                                            (patchisize namedFieldType)

namedFieldToVarBangType :: NamedField -> VarBangTypeQ
namedFieldToVarBangType (NamedField {..})
  = varBangType namedFieldName (bangType defaultBang namedFieldType)


data SumCon = SumCon
  { sumConName   :: Name
  , sumConFields :: [AnonymousField]
  }

dataConToSumCon :: DataCon -> Maybe SumCon
dataConToSumCon (DataCon {..}) = do
  -- existential types are not supported
  guard (null dcTvs)
  guard (null dcCxt)

  fields <- for dcFields $ \(maybeName, type_) -> do
    dataConFieldToAnonymousField (maybeName, pure type_)
  pure $ SumCon dcName fields

instance HasFieldTypes SumCon where
  fieldTypes (SumCon {..}) = fieldTypes sumConFields

sumConToCon :: SumCon -> Q Con
sumConToCon (SumCon {..})
  = normalC sumConName (anonymousFieldToBangType <$> sumConFields)


data ProductCon = ProductCon
  { productConName :: Name
  , productConFields :: [NamedField]
  }

dataConToProductCon :: DataCon -> Maybe ProductCon
dataConToProductCon (DataCon {..}) = do
  -- existential types are not supported
  guard (null dcTvs)
  guard (null dcCxt)

  fields <- for dcFields $ \(maybeName, type_) -> do
    dataConFieldToNamedField (maybeName, pure type_)
  pure $ ProductCon dcName fields

instance HasFieldTypes ProductCon where
  fieldTypes (ProductCon {..}) = fieldTypes productConFields

instance Patchisize ProductCon where
  patchisize (ProductCon {..}) = ProductCon (prefixedName "Patch" productConName)
                                            (patchisize <$> productConFields)

productConToCon :: ProductCon -> ConQ
productConToCon (ProductCon {..})
  = recC productConName (namedFieldToVarBangType <$> productConFields)



data SumType = SumType
  { sumTypeCon      :: TypeCon
  , sumTypeDataCons :: [SumCon]
  }

dataTypeToSumType :: DataType -> Maybe SumType
dataTypeToSumType (DataType {..}) = do
  guard (null dtCxt) -- datatype contexts are not supported
  constructors <- traverse dataConToSumCon dtCons
  pure $ SumType (TypeCon dtName dtTvs) constructors

instance IsType SumType where
  asType = asType . sumTypeCon

instance HasFieldTypes SumType where
  fieldTypes (SumType {..}) = fieldTypes sumTypeDataCons

-- |
-- > data Either a b
-- >   = Left  a
-- >   | Right b
--
-- to
--
-- > data PatchEither
-- >   = ReplaceEither (Either a b)
-- >   | PatchEither (Patch a) (Patch b)
instance Patchisize SumType where
  patchisize (SumType {..})
    = SumType (patchisize sumTypeCon)
              [ SumCon (prefixedName "Replace" name)
                       [AnonymousField $ asType sumTypeCon]
              , SumCon (prefixedName "Patch" name)
                       (AnonymousField . patchisize <$> fieldTypes sumTypeDataCons)
              ]
    where
      name :: Name
      name = typeConName sumTypeCon

sumTypeDec :: SumType -> DecQ
sumTypeDec (SumType {..}) = dataD (cxt [])
                                  (typeConName sumTypeCon)
                                  (PlainTV <$> typeConTvs sumTypeCon)
                                  Nothing
                                  (sumConToCon <$> sumTypeDataCons)
                                  (cxt [])

sumTypeInfo :: SumType -> InfoQ
sumTypeInfo = tyConI . sumTypeDec

instance TopLevel SumType where
  declare = declare . sumTypeDec


data ProductType = ProductType
  { productTypeCon     :: TypeCon
  , productTypeDataCon :: ProductCon
  }

dataTypeToProductType :: DataType -> Maybe ProductType
dataTypeToProductType (DataType {..}) = do
  guard (null dtCxt) -- datatype contexts are not supported
  [constructor] <- traverse dataConToProductCon dtCons
  pure $ ProductType (TypeCon dtName dtTvs) constructor

instance IsType ProductType where
  asType = asType . productTypeCon

instance HasFieldTypes ProductType where
  fieldTypes (ProductType {..}) = fieldTypes productTypeDataCon

-- |
-- > data User = MkUser
-- >   { _userName :: Text
-- >   , _userAge  :: Int
-- >   }
--
-- to
--
-- > data PatchUser = PatchMkUser
-- >   { _patchUserName :: Patch Text
-- >   , _patchUserAge  :: Patch Int
-- >   }
instance Patchisize ProductType where
  patchisize (ProductType {..}) = ProductType (patchisize productTypeCon)
                                              (patchisize productTypeDataCon)

productTypeDec :: ProductType -> DecQ
productTypeDec (ProductType {..}) = dataD (cxt [])
                                          (typeConName productTypeCon)
                                          (PlainTV <$> typeConTvs productTypeCon)
                                          Nothing
                                          [productConToCon productTypeDataCon]
                                          (cxt [])

productTypeInfo :: ProductType -> InfoQ
productTypeInfo = tyConI . productTypeDec

instance TopLevel ProductType where
  declare  = declare . productTypeDec


data POAD
  = SumPoad     SumType
  | ProductPoad ProductType

dataTypeToPoad :: DataType -> Maybe POAD
dataTypeToPoad dataType = (SumPoad     <$> dataTypeToSumType     dataType)
                      <|> (ProductPoad <$> dataTypeToProductType dataType)

reifyPoad :: Name -> Q POAD
reifyPoad typeName = do
  dataType <- reifyDataType typeName
  case dataTypeToPoad dataType of
    Just poad -> pure poad
    Nothing -> fail
             $ printf "The type %s uses features which aren't supported by this TemplateHaskell transformation. We only support products with named fields and sums with anonymous fields."
                      (show typeName)

instance IsType POAD where
  asType (SumPoad     x) = asType x
  asType (ProductPoad x) = asType x

instance HasFieldTypes POAD where
  fieldTypes (SumPoad     x) = fieldTypes x
  fieldTypes (ProductPoad x) = fieldTypes x

instance Patchisize POAD where
  patchisize (SumPoad     x) = SumPoad     $ patchisize x
  patchisize (ProductPoad x) = ProductPoad $ patchisize x

instance TopLevel POAD where
  declare (SumPoad     x) = declare x
  declare (ProductPoad x) = declare x



tyConI :: DecQ -> InfoQ
tyConI = fmap TyConI

ctorE :: Name -> [ExpQ] -> ExpQ
ctorE = foldl' appE . conE

ctorT :: Name -> [TypeQ] -> TypeQ
ctorT = foldl' appT . conT

diffT :: TypeQ -> PredQ
diffT t = [t|Diff $t|]


standaloneDeriving :: (IsType a, HasFieldTypes a) => Name -> a -> DecQ
standaloneDeriving className a
  = standaloneDerivD (cxt $ fmap mkConstraint $ fieldTypes a)
                     (mkConstraint $ asType a)
  where
    mkConstraint :: TypeQ -> TypeQ
    mkConstraint = appT (conT className)


declareInstance :: TopLevel a => TypeQ -> a -> DecsQ
declareInstance tp body = (:) <$> (InstanceD Nothing [] <$> tp <*> declare body)
                              <*> pure []


-- |
-- Generate a 'Patch' type and a 'Diff' implementation for the given type.
--
-- For example, if the type 'User' is defined as follows:
--
-- > data User = MkUser
-- >   { _userName :: Text
-- >   , _userAge  :: Int
-- >   }
-- >   deriving (Eq, Show)
--
-- Then `makeStructuredPatch ''User` will generate the following code:
--
-- > data PatchUser = PatchMkUser
-- >   { _patchUserName :: Patch Text
-- >   , _patchUserAge  :: Patch Int
-- >   }
-- >
-- > deriving instance Show PatchUser
-- > deriving instance Eq   PatchUser
-- >
-- > makeLenses ''PatchUser
--
-- (and ideally the following code, but not yet)
--
-- > _PatchUserName :: Review PatchUser (Patch Text)
-- > _PatchUserName = unto $ flip PatchMkUser mempty
-- >
-- > _PatchUserAge :: Review PatchUser (Patch Int)
-- > _PatchUserAge = unto $ PatchMkUser mempty
-- >
-- > instance Monoid PatchUser where
-- >   mempty = PatchMkUser mempty mempty
-- >   PatchMkUser name12 age12 `mappend` PatchMkUser name23 age23 = PatchMkUser name13 age13
-- >     where
-- >       name13 = name12 <> name23
-- >       age13 = age12 <> age23
-- >
-- > instance Diff User where
-- >   type Patch User = PatchUser
-- >   diff (MkUser name1 age1) (MkUser name2 age2) = PatchMkUser name12 age12
-- >     where
-- >       name12 = diff name1 name2
-- >       age12 = diff age1 age2
-- >   patch (PatchMkUser name12 age12) (MkUser name1 age1) = MkUser name2 age2
-- >     where
-- >       name2 = patch name12 name1
-- >       age2 = patch age12 age1
--
-- Sum types are supported too, here's what `makeStructuredPatch ''Either` generates:
--
-- > data PatchEither a b
-- >   = ReplaceEither (Either a b)
-- >   | PatchEither (Patch a) (Patch b)
-- >
-- > deriving instance (Show a, Show b, Show (Patch a), Show (Patch b)) => Show (PatchEither a b)
-- > deriving instance (Eq   a, Eq   b, Eq   (Patch a), Eq   (Patch b)) => Eq   (PatchEither a b)
-- >
-- > makePrisms ''PatchEither
--
-- (and ideally the following code, but not yet)
--
-- > _PatchLeft :: Diff b => Review (PatchEither a b) (Patch a)
-- > _PatchLeft = unto $ flip PatchEither mempty
-- >
-- > _PatchRight :: Diff a => Review (PatchEither a b) (Patch b)
-- > _PatchRight = unto $ PatchEither mempty
-- >
-- > instance (Diff a, Diff b) => Monoid (PatchEither a b) where
-- >   mempty = PatchEither mempty mempty
-- >   _                        `mappend` p@ReplaceEither {}  = p
-- >   ReplaceEither (Left  a2) `mappend` PatchEither a23 _   = ReplaceEither . Left  . patch a23 $ a2
-- >   ReplaceEither (Right b2) `mappend` PatchEither _   b23 = ReplaceEither . Right . patch b23 $ b2
-- >   PatchEither a12 b12      `mappend` PatchEither a23 b23 = PatchEither (a12 <> a23) (b12 <> b23)
-- >
-- > instance (Diff a, Diff b) => Diff (Either a b) where
-- >   type Patch (Either a b) = PatchEither a b
-- >
-- >   diff (Left  a1) (Left  a2) = PatchEither (diff a1 a2) mempty
-- >   diff (Right b1) (Right b2) = PatchEither mempty (diff b1 b2)
-- >   diff _          x          = ReplaceEither x
-- >
-- >   patch (ReplaceEither x)   _          = x
-- >   patch (PatchEither a12 _) (Left  a1) = Left  . patch a12 $ a1
-- >   patch (PatchEither _ b12) (Right b1) = Right . patch b12 $ b1
makeStructuredPatch :: Name -> Q [Dec]
makeStructuredPatch typeName = do
  let Name (OccName poadName) _ = typeName
  poad <- reifyPoad typeName

  let n = length (fieldTypes poad)
  name1s  <- replicateM n (newName "field1")   -- [name1, age1]
  name2s  <- replicateM n (newName "field2")   -- [name2, age2]
  name12s <- replicateM n (newName "field12")  -- [name12, age12]
  name23s <- replicateM n (newName "field23")  -- [name23, age23]
  name13s <- replicateM n (newName "field13")  -- [name13, age13]

  let patchisizedPoad :: POAD
      patchisizedPoad = patchisize poad

  let poadType = asType poad
  let patchType = asType patchisizedPoad

  let derivingEq   = standaloneDeriving ''Eq   patchisizedPoad
  let derivingShow = standaloneDeriving ''Show patchisizedPoad

  let makeOptics = case patchisizedPoad of
        SumPoad     x -> makePrismsForInfo =<< sumTypeInfo     x
        ProductPoad x -> makeLensesForInfo =<< productTypeInfo x

  let remainingDecls = case patchisizedPoad of
        SumPoad {} -> do
          -- TODO
          declare ()

        ProductPoad {} -> do
          let asCtorName :: POAD -> Name
              asCtorName (ProductPoad (ProductType _ (ProductCon name _))) = name
              asCtorName (SumPoad {}) = error "never happens: we know poad and patchisizedPoad are products."
          let ctorName      = asCtorName poad             -- MkUser
          let patchCtorName = asCtorName patchisizedPoad  -- PatchMkUser

          let pat1 = conP ctorName $ fmap varP name1s         -- { MkUser name1 age1 }
          let pat2 = conP ctorName $ fmap varP name2s         -- { MkUser name2 age2 }
          let pat12 = conP patchCtorName $ fmap varP name12s  -- { PatchMkUser name12 age12 }

          let exp2 = ctorE ctorName $ fmap varE name2s        -- MkUser name2 age2
          let exp12 = ctorE patchCtorName $ fmap varE name12s -- PatchMkUser name12 age12

          declare
            [ instanceD                                    -- instance
              (cxt $ fmap diffT $ fieldTypes poad)         --     (Monoid Text, Monoid Int)
              [t|Monoid $(asType patchisizedPoad)|]        --     => Monoid PatchUser where
              [ funD 'mempty                               --   mempty
                [ clause []                                --
                         ( normalB                         --
                         $ ctorE patchCtorName             --     = PatchUser
                         $ replicate n (varE 'mempty)      --         mempty
                         )                                 --         mempty
                         []                                --
                ]                                          --
              , funD 'mappend                              --   mappend
                [ clause [ conP patchCtorName              --     (PatchMkUser
                         $ fmap varP name12s               --       name12 age12)
                         , conP patchCtorName              --     (PatchMkUser
                         $ fmap varP name23s               --       name23 age23)
                         ]                                 --
                         ( normalB                         --
                         $ ctorE patchCtorName             --     = PatchMkUser
                         $ fmap varE name13s               --         name13 age13
                         )                                 --
                $ execWriter $ do                          --     where
                    for_ (zip3 name13s name12s name23s)    --
                       $ \(name13, name12, name23) -> do   --
                      tell [valD (varP name13)             --       name13
                                 ( normalB                 --
                                 $ [| $(varE name12)       --         = name13
                                   <> $(varE name23)       --        <> name23
                                    |]                     --
                                 )                         --
                                 []]                       --
                ]                                          --
              ]                                            --
            , instanceD                                    -- instance
              (cxt $ fmap diffT $ fieldTypes poad)         --     (Diff Text, Diff Int)
              [t|Diff $(asType poad)|]                     --     => Diff User where
              [ tySynInstD ''Patch                         --   type Patch
              $ tySynEqn [asType poad]                     --          User
                         (asType patchisizedPoad)          --      = PatchUser
              , funD 'diff                                 --   diff
                [ clause [ pat1                            --     (MkUser name1 age1)
                         , pat2                            --     (MkUser name2 age2)
                         ]                                 --
                         (normalB exp12)                   --     = PatchMkUser name12 age12
                $ execWriter $ do                          --     where
                    for_ (zip3 name1s name2s name12s)      --
                       $ \(name1, name2, name12) -> do     --
                      tell [valD (varP name12)             --       name12
                                 ( normalB                 --
                                 $ [|diff $(varE name1)    --         = diff name1
                                          $(varE name2)    --                name2
                                    |]                     --
                                 )                         --
                                 []]                       --
                ]                                          --
              , funD 'patch                                --   patch
                [ clause [ pat12                           --     (PatchMkUser name12 age12)
                         , pat1                            --     (MkUser name1 age1)
                         ]                                 --
                         (normalB exp2)                    --   = MkUser name2 age2
                $ execWriter $ do                          --   where
                    for_ (zip3 name1s name2s name12s)      --
                       $ \(name1, name2, name12) -> do     --
                      tell [valD (varP name2)              --     name2
                                 ( normalB                 --
                                 $ [|patch $(varE name12)  --       = patch name12
                                           $(varE name1)   --               name1
                                    |]
                                 )
                                 []]
                ]
              ]
            ]

  declare
    [ declare patchisizedPoad
    , declare derivingEq
    , declare derivingShow
    , declare makeOptics
    , declare remainingDecls
    ]
