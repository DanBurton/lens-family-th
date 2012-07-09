{-# LANGUAGE TemplateHaskell, Rank2Types #-}

-- | Derive lenses for "Lens.Family2".
-- 
-- Example usage:
-- 
-- 
-- > {-# LANGUAGE TemplateHaskell, Rank2Types #-}
-- > 
-- > import Lens.Family2
-- > import Lens.Family2.TH
-- > 
-- > data Foo a = Foo { _bar :: Int, _baz :: a }
-- >            deriving (Show, Read, Eq, Ord)
-- > $(mkLenses ''Foo)
-- 
module Lens.Family2.TH (
    mkLenses
  , mkLensesBy
  , mkLensesFor
  ) where

import Language.Haskell.TH
import Lens.Family.THCore


-- | Derive lenses for the record selectors in 
-- a single-constructor data declaration,
-- or for the record selector in a newtype declaration.
-- Lenses will only be generated for record fields which
-- are prefixed with an underscore.
-- 
-- Example usage:
-- 
-- $(mkLenses ''Foo)
mkLenses :: Name -> Q [Dec]
mkLenses = mkLensesBy defaultNameTransform


-- | Derive lenses with the provided name transformation
-- and filtering function. Produce @Just lensName@ to generate a lens
-- of the resultant name, or @Nothing@ to not generate a lens
-- for the input record name.
-- 
-- Example usage:
-- 
-- > $(mkLensesBy (\n -> Just (n ++ "L")) ''Foo)
mkLensesBy :: (String -> Maybe String) -> Name -> Q [Dec]
mkLensesBy = deriveLenses deriveLensSig


-- | Derive lenses, specifying explicit pairings of @(fieldName, lensName)@.
-- 
-- Example usage:
-- 
-- > $(mkLensesFor [("_foo", "fooLens"), ("bar", "lbar")] ''Foo)
mkLensesFor :: [(String, String)] -> Name -> Q [Dec]
mkLensesFor fields = mkLensesBy (`lookup` fields)


-- TODO
deriveLensSig :: Name -> LensTypeInfo -> ConstructorFieldInfo -> Q [Dec]
deriveLensSig _ _ _ = return []

