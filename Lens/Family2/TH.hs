{-# LANGUAGE TemplateHaskell, Rank2Types #-}

-- | Derive lenses for "Lens.Family2".
-- 
-- Example usage:
-- 
-- 
-- > {-# LANGUAGE TemplateHaskell, Rank2Types -#}
-- > 
-- > import Lens.Family2
-- > import Lens.Family2.TH
-- > 
-- > data Foo a = Foo { _bar :: Int, _baz :: a }
-- >            deriving (Show, Read, Eq, Ord)
-- > $(mkLenses ''Foo)
-- 
module Lens.Family2.TH (mkLenses, mkLensesBy) where

import Language.Haskell.TH
import Lens.Family.THCore


-- | Derive lenses for the record selectors in 
-- a single-constructor data declaration,
-- or for the record selector in a newtype declaration.
mkLenses :: Name -> Q [Dec]
mkLenses = mkLensesBy defaultNameTransform

-- | Derive lenses with the provided name transformation function.
mkLensesBy :: (String -> String) -> Name -> Q [Dec]
mkLensesBy = deriveLenses deriveLensSig


-- TODO
deriveLensSig :: Name -> LensTypeInfo -> ConstructorFieldInfo -> Q [Dec]
deriveLensSig _ _ _ = return []

