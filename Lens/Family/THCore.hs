{-# LANGUAGE TemplateHaskell #-}

-- | The shared functionality behind Lens.Family.TH and Lens.Family2.TH.
module Lens.Family.THCore (
   defaultNameTransform
  , LensTypeInfo
  , ConstructorFieldInfo
  , deriveLenses
  ) where

import Language.Haskell.TH
import Data.Char (toLower)

-- | By default, if the field name begins with an underscore,
-- then the underscore will simply be removed (and the new first character
-- lowercased if necessary). Otherwise, the suffix "Lens" will be added.
defaultNameTransform :: String -> String
defaultNameTransform ('_':c:rest) = toLower c : rest
defaultNameTransform n = n ++ "Lens"

-- | Information about the larger type the lens will operate on.
type LensTypeInfo = (Name, [TyVarBndr])

-- | Information about the smaller type the lens will operate on.
type ConstructorFieldInfo = (Name, Strict, Type)


-- | The true workhorse of lens derivation. This macro is parameterized
-- by a macro that derives signatures, as well as a function that
-- transforms names.
deriveLenses ::
     (Name -> LensTypeInfo -> ConstructorFieldInfo -> Q [Dec])
     -- ^ the signature deriver
  -> (String -> String)
     -- ^ the name transformer
  -> Name -> Q [Dec]
deriveLenses sigDeriver nameTransform datatype = do
  typeInfo          <- extractLensTypeInfo datatype
  let derive1 = deriveLens sigDeriver nameTransform typeInfo
  constructorFields <- extractConstructorFields datatype
  concat `fmap` mapM derive1 constructorFields


extractLensTypeInfo :: Name -> Q LensTypeInfo
extractLensTypeInfo datatype = do
  let datatypeStr = nameBase datatype
  i <- reify datatype
  return $ case i of
    TyConI (DataD    _ n ts _ _) -> (n, ts)
    TyConI (NewtypeD _ n ts _ _) -> (n, ts)
    _ -> error $ "Can't derive Lens for: "  ++ datatypeStr
              ++ ", type name required."


extractConstructorFields :: Name -> Q [ConstructorFieldInfo]
extractConstructorFields datatype = do
  let datatypeStr = nameBase datatype
  i <- reify datatype
  return $ case i of
    TyConI (DataD    _ _ _ [RecC _ fs] _) -> fs
    TyConI (NewtypeD _ _ _ (RecC _ fs) _) -> fs
    TyConI (DataD    _ _ _ [_]         _) ->
      error $ "Can't derive Lens without record selectors: " ++ datatypeStr
    TyConI NewtypeD{} ->
      error $ "Can't derive Lens without record selectors: " ++ datatypeStr
    TyConI TySynD{} ->
      error $ "Can't derive Lens for type synonym: " ++ datatypeStr
    TyConI DataD{} ->
      error $ "Can't derive Lens for tagged union: " ++ datatypeStr
    _ ->
      error $ "Can't derive Lens for: "  ++ datatypeStr
           ++ ", type name required."


-- Derive a lens for the given record selector
-- using the given name transformation function.
deriveLens :: (Name -> LensTypeInfo -> ConstructorFieldInfo -> Q [Dec])
           -> (String -> String)
           -> LensTypeInfo -> ConstructorFieldInfo -> Q [Dec]
deriveLens sigDeriver nameTransform ty field = do
  let (fieldName, _fieldStrict, _fieldType) = field
      (_tyName, _tyVars) = ty  -- just to clarify what's here
      lensName = mkName $ nameTransform $ nameBase fieldName
  sig <- sigDeriver lensName ty field
  body <- deriveLensBody lensName fieldName
  return $ sig ++ [body]


-- Given a record field name,
-- produces a single function declaration:
-- lensName f a = (\x -> a { field = x }) `fmap` f (field a)
deriveLensBody :: Name -> Name -> Q Dec
deriveLensBody lensName fieldName = funD lensName [defLine]
  where
    a = mkName "a"
    f = mkName "f"
    defLine = clause pats (normalB body) []
    pats = [varP f, varP a]
    body = [| (\x -> $(record a fieldName [|x|]))
              `fmap` $(appE (varE f) (appE (varE fieldName) (varE a)))
            |]
    record rec fld val = val >>= \v -> recUpdE (varE rec) [return (fld, v)]

