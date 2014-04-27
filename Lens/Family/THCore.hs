{-# LANGUAGE TemplateHaskell #-}

-- | The shared functionality behind Lens.Family.TH and Lens.Family2.TH.
module Lens.Family.THCore (
   defaultNameTransform
  , LensTypeInfo
  , ConstructorFieldInfo
  , deriveLenses
  , makeTraversals
  ) where

import Language.Haskell.TH
import Control.Applicative (pure)
import Data.Char (toLower)

-- | By default, if the field name begins with an underscore,
-- then the underscore will simply be removed (and the new first character
-- lowercased if necessary).
defaultNameTransform :: String -> Maybe String
defaultNameTransform ('_':c:rest) = Just $ toLower c : rest
defaultNameTransform _ = Nothing


-- | Information about the larger type the lens will operate on.
type LensTypeInfo = (Name, [TyVarBndr])

-- | Information about the smaller type the lens will operate on.
type ConstructorFieldInfo = (Name, Strict, Type)


-- | The true workhorse of lens derivation. This macro is parameterized
-- by a macro that derives signatures, as well as a function that
-- filters and transforms names. Producing Nothing means that
-- a lens should not be generated for the provided name.
deriveLenses ::
     (Name -> LensTypeInfo -> ConstructorFieldInfo -> Q [Dec])
     -- ^ the signature deriver
  -> (String -> Maybe String)
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
           -> (String -> Maybe String)
           -> LensTypeInfo -> ConstructorFieldInfo -> Q [Dec]
deriveLens sigDeriver nameTransform ty field = do
  let (fieldName, _fieldStrict, _fieldType) = field
      (_tyName, _tyVars) = ty  -- just to clarify what's here
  case nameTransform (nameBase fieldName) of
    Nothing       -> return []
    Just lensNameStr -> do
      let lensName = mkName lensNameStr
      sig  <- sigDeriver lensName ty field
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

makeTraversals :: Name -> Q [Dec]
makeTraversals = deriveTraversals (\s -> Just ('_':s))

deriveTraversals :: (String -> Maybe String) -> Name -> Q [Dec]
deriveTraversals nameTransform name = do
  typeInfo <- extractLensTypeInfo name
  let derive1 = deriveTraversal nameTransform typeInfo
  constructors <- extractConstructorInfo name
  concat `fmap` mapM derive1 constructors


extractConstructorInfo :: Name -> Q [Con]
extractConstructorInfo datatype = do
  let datatypeStr = nameBase datatype
  i <- reify datatype
  return $ case i of
    TyConI (DataD    _ _ _ [] _)
      -> error $ "Traversal derivation not yet supported: "
         ++ "empty constructor in: " ++ datatypeStr
    TyConI (DataD    _ _ _ fs _) -> fs
    TyConI (NewtypeD _ _ _ f  _) -> [f]
    _ -> error $ "Can't derive traversal for: " ++ datatypeStr


deriveTraversal :: (String -> Maybe String) -> LensTypeInfo -> Con -> Q [Dec]
deriveTraversal nameTransform ty con = do
  let (tyName, _tyVars) = ty  -- just to clarify what's here
      (cName, cTys) = case con of
        NormalC n tys -> (n, tys)
        RecC n tys -> (n, map (\(_n, s, t) -> (s, t)) tys)
        InfixC _ n _
          -> error $ "Traversal derivation not yet supported: "
             ++ "infix constructor: " ++ nameBase n
        ForallC _ _ _
          -> error $ "Traversal derivation not supported: "
             ++ "forall'd constructor in: " ++ nameBase tyName
  cTy <- case cTys of
    [t] -> return t
    -- TODO: this should be pretty easy to implement
    _ -> error $ "Traversal derivation not yet supported: "
         ++ "product constructor: " ++ nameBase cName
  case nameTransform (nameBase cName) of
    Nothing       -> return []
    Just lensNameStr -> do
      let lensName = mkName lensNameStr
      sig  <- return [] -- TODO
      body <- deriveTraversalBody lensName cName
      return $ sig ++ [body]


deriveTraversalBody :: Name -> Name -> Q Dec
deriveTraversalBody lensName constructorName =
  funD lensName [defLine, fallback] where
    x = mkName "x"
    t = mkName "t"
    k = mkName "k"
    defLine = clause defPats (normalB defBody) []
    defPats = [varP k, conP constructorName [varP x]]
    defBody = [| $(conE constructorName)
                 `fmap` $(appE (varE k) (varE x))
               |]
    fallback = clause fallbackPats (normalB fallbackBody) []
    fallbackPats = [wildP, varP t]
    fallbackBody = [| pure $(varE t) |]

