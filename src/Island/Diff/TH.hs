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


patchisizeType :: Type -> Type
patchisizeType = AppT (ConT ''Patch)


data AnonymousField = AnonymousField
  { anonymousFieldType :: Type
  }
  deriving (Eq, Show)

dataConFieldToAnonymousField :: (Maybe Name, Type) -> Maybe AnonymousField
dataConFieldToAnonymousField (nameMay, type_) = do
  guard (nameMay == Nothing)
  pure $ AnonymousField type_

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

patchisizeSumCon :: SumCon -> SumCon
patchisizeSumCon (SumCon {..})
  = SumCon (prefixedName "Patch" sumConName)
           (patchisizeAnonymousField <$> sumConFields)

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

patchisizeProductCon :: ProductCon -> ProductCon
patchisizeProductCon (ProductCon {..})
  = ProductCon (prefixedName "Patch" productConName)
               (patchisizeNamedField <$> productConFields)

productConToCon :: ProductCon -> Con
productConToCon (ProductCon {..})
  = RecC productConName (namedFieldToVarBangType <$> productConFields)


data SumType = SumType
  { sumTypeName :: Name
  , sumTypeCons :: [SumCon]
  }
  deriving (Eq, Show)

dataTypeToSumType :: DataType -> Maybe SumType
dataTypeToSumType (DataType {..}) = do
  guard (null dtTvs) -- TODO: support type variables
  guard (null dtCxt) -- datatype contexts are not supported
  constructors <- traverse dataConToSumCon dtCons
  pure $ SumType dtName constructors

patchisizeSumType :: SumType -> SumType
patchisizeSumType = error "unimplemented"

instance TopLevel SumType where
  declare (SumType {..}) = declare
                         $ DataD []
                                 sumTypeName
                                 []
                                 Nothing
                                 (sumConToCon <$> sumTypeCons)
                                 []


data ProductType = ProductType
  { productTypeName :: Name
  , productTypeCon  :: ProductCon
  }
  deriving (Eq, Show)

dataTypeToProductType :: DataType -> Maybe ProductType
dataTypeToProductType (DataType {..}) = do
  guard (null dtTvs) -- TODO: support type variables
  guard (null dtCxt) -- datatype contexts are not supported
  [constructor] <- traverse dataConToProductCon dtCons
  pure $ ProductType dtName constructor

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
  = ProductType (prefixedName "Patch" productTypeName)
                (patchisizeProductCon productTypeCon)

instance TopLevel ProductType where
  declare (ProductType {..}) = declare
                             $ DataD []
                                     productTypeName
                                     []
                                     Nothing
                                     [productConToCon productTypeCon]
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

patchisizePoad :: POAD -> POAD
patchisizePoad (SumPoad     x) = SumPoad     $ patchisizeSumType     x
patchisizePoad (ProductPoad x) = ProductPoad $ patchisizeProductType x

instance TopLevel POAD where
  declare (SumPoad     x) = declare x
  declare (ProductPoad x) = declare x





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
-- >   deriving (Eq, Show)
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
makeStructuredPatch :: Name -> Q [Dec]
makeStructuredPatch typeName = do
  patchisizedPoad <- patchisizePoad <$> reifyPoad typeName
  declare patchisizedPoad
