lens-family-th
==============

Template Haskell to generate lenses for lens-family and lens-family-core.

Usage:

    {-# LANGUAGE TemplateHaskell -#}
    import Lens.Family.TH

    data Foo = Foo { _bar :: Int, _baz :: String }
             deriving (Show, Read, Eq, Ord)
    $(mkLenses ''Foo)

This will create lenses `bar` and `baz`.