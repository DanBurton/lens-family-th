lens-family-th
==============

Template Haskell to generate lenses for lens-family and lens-family-core.

Usage:

    {-# LANGUAGE TemplateHaskell, Rank2Types -#}

    import Lens.Family2
    import Lens.Family2.TH

    data Foo a = Foo { _bar :: Int, _baz :: a }
             deriving (Show, Read, Eq, Ord)
    $(mkLenses ''Foo)

This will create lenses `bar` and `baz`.
