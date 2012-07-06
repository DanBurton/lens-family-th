{-# LANGUAGE TemplateHaskell, Rank2Types #-}

module Lens.Family2.TH (mkLenses, mkLensesBy) where

import Lens.Family.THCore
import Language.Haskell.TH

mkLenses :: Name -> Q [Dec]
mkLenses = mkLensesBy (drop 1)

mkLensesBy :: (String -> String) -> Name -> Q [Dec]
mkLensesBy = deriveLenses deriveLensSig

-- TODO
deriveLensSig :: Name -> LensTypeInfo -> ConstructorFieldInfo -> Q [Dec]
deriveLensSig _ _ _ = return []

