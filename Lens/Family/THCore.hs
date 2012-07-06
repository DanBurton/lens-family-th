{-# LANGUAGE TemplateHaskell #-}

module Lens.Family.THCore (
    LensTypeInfo
  , ConstructorFieldInfo
  , deriveLenses
  ) where

import Language.Haskell.TH


type LensTypeInfo = (Name, [TyVarBndr])
type ConstructorFieldInfo = (Name, Strict, Type)

deriveLenses :: (Name -> LensTypeInfo -> ConstructorFieldInfo -> Q [Dec])
             -> (String -> String) -> Name -> Q [Dec]
deriveLenses sigDeriver nameTransform datatype = do
  constructorFields <- extractConstructorFields datatype
  typeInfo <- extractLensTypeInfo datatype
  concat `fmap`
    mapM (deriveLens sigDeriver nameTransform typeInfo) constructorFields


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
      (_tyName, _tyVars) = ty
      lensName = mkName $ nameTransform $ nameBase fieldName
  body <- deriveLensBody lensName fieldName
  sig <- sigDeriver lensName ty field
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

