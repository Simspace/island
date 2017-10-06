{-# OPTIONS -Wno-unused-top-binds -Wno-unused-local-binds #-}
module Island.Diff.TH (makeStructuredPatch) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List.Extra
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Printf
import TH.ReifySimple

import Island.Diff


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


defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness


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
  asType :: a -> Type


class HasFieldTypes a where
  fieldTypes :: a -> [Type]

instance HasFieldTypes () where
  fieldTypes () = []

instance HasFieldTypes a => HasFieldTypes [a] where
  fieldTypes = concatMap fieldTypes


patchisizeType :: Type -> Type
patchisizeType = AppT (ConT ''Patch)


data TypeCon = TypeCon
  { typeConName :: Name
  , typeConTvs  :: [Name]  -- ^ type variables
  }
  deriving (Eq, Show)

instance IsType TypeCon where
  asType (TypeCon {..}) = foldl' (\t tv -> t `AppT` VarT tv)
                                 (ConT typeConName)
                                 typeConTvs

patchisizeTypeCon :: TypeCon -> TypeCon
patchisizeTypeCon (TypeCon {..})
  = TypeCon (prefixedName "Patch" typeConName)
            typeConTvs


data AnonymousField = AnonymousField
  { anonymousFieldType :: Type
  }
  deriving (Eq, Show)

dataConFieldToAnonymousField :: (Maybe Name, Type) -> Maybe AnonymousField
dataConFieldToAnonymousField (nameMay, type_) = do
  guard (nameMay == Nothing)
  pure $ AnonymousField type_

instance HasFieldTypes AnonymousField where
  fieldTypes (AnonymousField {..}) = [anonymousFieldType]

patchisizeAnonymousField :: AnonymousField -> AnonymousField
patchisizeAnonymousField (AnonymousField {..})
  = AnonymousField (patchisizeType anonymousFieldType)

anonymousFieldToBangType :: AnonymousField -> BangType
anonymousFieldToBangType (AnonymousField {..})
  = (defaultBang, anonymousFieldType)


data NamedField = NamedField
  { namedFieldName :: Name
  , namedFieldType :: Type
  }
  deriving (Eq, Show)

dataConFieldToNamedField :: (Maybe Name, Type) -> Maybe NamedField
dataConFieldToNamedField (nameMay, type_) = do
  name <- nameMay
  pure $ NamedField name type_

instance HasFieldTypes NamedField where
  fieldTypes (NamedField {..}) = [namedFieldType]

patchisizeNamedField :: NamedField -> NamedField
patchisizeNamedField (NamedField {..})
  = NamedField (prefixedName "_patch" namedFieldName)
               (patchisizeType namedFieldType)

namedFieldToVarBangType :: NamedField -> VarBangType
namedFieldToVarBangType (NamedField {..})
  = (namedFieldName, defaultBang, namedFieldType)


data SumCon = SumCon
  { sumConName   :: Name
  , sumConFields :: [AnonymousField]
  }
  deriving (Eq, Show)

dataConToSumCon :: DataCon -> Maybe SumCon
dataConToSumCon (DataCon {..}) = do
  -- existential types are not supported
  guard (null dcTvs)
  guard (null dcCxt)

  fields <- traverse dataConFieldToAnonymousField dcFields
  pure $ SumCon dcName fields

instance HasFieldTypes SumCon where
  fieldTypes (SumCon {..}) = fieldTypes sumConFields

sumConToCon :: SumCon -> Con
sumConToCon (SumCon {..})
  = NormalC sumConName (anonymousFieldToBangType <$> sumConFields)


data ProductCon = ProductCon
  { productConName :: Name
  , productConFields :: [NamedField]
  }
  deriving (Eq, Show)

dataConToProductCon :: DataCon -> Maybe ProductCon
dataConToProductCon (DataCon {..}) = do
  -- existential types are not supported
  guard (null dcTvs)
  guard (null dcCxt)

  fields <- traverse dataConFieldToNamedField dcFields
  pure $ ProductCon dcName fields

instance HasFieldTypes ProductCon where
  fieldTypes (ProductCon {..}) = fieldTypes productConFields

patchisizeProductCon :: ProductCon -> ProductCon
patchisizeProductCon (ProductCon {..})
  = ProductCon (prefixedName "Patch" productConName)
               (patchisizeNamedField <$> productConFields)

productConToCon :: ProductCon -> Con
productConToCon (ProductCon {..})
  = RecC productConName (namedFieldToVarBangType <$> productConFields)



data SumType = SumType
  { sumTypeCon      :: TypeCon
  , sumTypeDataCons :: [SumCon]
  }
  deriving (Eq, Show)

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
patchisizeSumType :: SumType -> SumType
patchisizeSumType (SumType {..})
  = SumType (patchisizeTypeCon sumTypeCon)
            [ SumCon (prefixedName "Replace" name)
                     [AnonymousField $ asType sumTypeCon]
            , SumCon (prefixedName "Patch" name)
                     (AnonymousField . patchisizeType <$> fieldTypes sumTypeDataCons)
            ]
  where
    name :: Name
    name = typeConName sumTypeCon

instance TopLevel SumType where
  declare (SumType {..}) = declare
                         $ DataD []
                                 (typeConName sumTypeCon)
                                 (PlainTV <$> typeConTvs sumTypeCon)
                                 Nothing
                                 (sumConToCon <$> sumTypeDataCons)
                                 []


data ProductType = ProductType
  { productTypeCon     :: TypeCon
  , productTypeDataCon :: ProductCon
  }
  deriving (Eq, Show)

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
-- > data User = User
-- >   { _userName :: Text
-- >   , _userAge  :: Int
-- >   }
--
-- to
--
-- > data PatchUser = PatchUser
-- >   { _patchUserName :: Patch Text
-- >   , _patchUserAge  :: Patch Int
-- >   }
patchisizeProductType :: ProductType -> ProductType
patchisizeProductType (ProductType {..})
  = ProductType (patchisizeTypeCon productTypeCon)
                (patchisizeProductCon productTypeDataCon)

instance TopLevel ProductType where
  declare (ProductType {..}) = declare
                             $ DataD []
                                     (typeConName productTypeCon)
                                     (PlainTV <$> typeConTvs productTypeCon)
                                     Nothing
                                     [productConToCon productTypeDataCon]
                                     []


data POAD
  = SumPoad     SumType
  | ProductPoad ProductType
  deriving (Eq, Show)

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

patchisizePoad :: POAD -> POAD
patchisizePoad (SumPoad     x) = SumPoad     $ patchisizeSumType     x
patchisizePoad (ProductPoad x) = ProductPoad $ patchisizeProductType x

instance TopLevel POAD where
  declare (SumPoad     x) = declare x
  declare (ProductPoad x) = declare x



standaloneDeriving :: (IsType a, HasFieldTypes a) => Type -> a -> Dec
standaloneDeriving constraint a
  = StandaloneDerivD (AppT constraint <$> fieldTypes a)
                     (AppT constraint  $  asType     a)



-- |
-- Generate a 'Patch' type and a 'Diff' implementation for the given type.
--
-- For example, if the type 'User' is defined as follows:
--
-- > data User = User
-- >   { _userName :: Text
-- >   , _userAge  :: Int
-- >   }
-- >   deriving (Eq, Show)
--
-- Then `makeStructuredPatch ''User` will generate the following code:
--
-- > data PatchUser = PatchUser
-- >   { _patchUserName :: Patch Text
-- >   , _patchUserAge  :: Patch Int
-- >   }
--
-- (and ideally the following code, but not yet)
--
-- > deriving instance Show PatchUser
-- > deriving instance Eq   PatchUser
-- >
-- > makeLenses ''PatchUser
-- >
-- > _PatchUserName :: Review PatchUser (Patch Text)
-- > _PatchUserName = unto $ flip PatchUser mempty
-- >
-- > _PatchUserAge :: Review PatchUser (Patch Int)
-- > _PatchUserAge = unto $ PatchUser mempty
-- >
-- > instance Monoid PatchUser where
-- >   mempty = PatchUser mempty mempty
-- >   PatchUser name12 age12 `mappend` PatchUser name23 age23 = PatchUser name13 age13
-- >     where
-- >       name13 = name12 <> name23
-- >       age13 = age12 <> age23
-- >
-- > instance Diff User where
-- >   type Patch User = PatchUser
-- >   diff (User name1 age1) (User name2 age2) = PatchUser name12 age12
-- >     where
-- >       name12 = diff name1 name2
-- >       age12 = diff age1 age2
-- >   patch (PatchUser name12 age12) (User name1 age1) = User name2 age2
-- >     where
-- >       name2 = patch name12 name1
-- >       age2 = patch age12 age1
--
-- Sum types are supported too, here's what `makeStructuredPatch ''Either` generates:
--
-- > data PatchEither a b
-- >   = ReplaceEither (Either a b)
-- >   | PatchEither (Patch a) (Patch b)
--
-- (and ideally the following code, but not yet)
--
-- > deriving instance (Show a, Show b, Show (Patch a), Show (Patch b)) => Show (PatchEither a b)
-- > deriving instance (Eq   a, Eq   b, Eq   (Patch a), Eq   (Patch b)) => Eq   (PatchEither a b)
-- >
-- > makeLenses ''PatchUser
-- >
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
  poad <- reifyPoad typeName

  let patchisizedPoad :: POAD
      patchisizedPoad = patchisizePoad poad

  let derivingEq   = standaloneDeriving (ConT ''Eq  ) patchisizedPoad
  let derivingShow = standaloneDeriving (ConT ''Show) patchisizedPoad

  declare
    [ declare patchisizedPoad
    , declare derivingEq
    , declare derivingShow
    ]
