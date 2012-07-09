lens-family-th
==============

Template Haskell to generate lenses for lens-family and lens-family-core.

Usage:

    {-# LANGUAGE TemplateHaskell, Rank2Types #-}

    import Lens.Family2
    import Lens.Family2.TH

    data Foo a = Foo { _bar :: Int, _baz :: a }
             deriving (Show, Read, Eq, Ord)
    $(mkLenses ''Foo)

This will create lenses `bar` and `baz`.

You can instead create these lenses by hand
as explained by documentation at [Lens.Family.Unchecked](http://hackage.haskell.org/packages/archive/lens-family-core/latest/doc/html/Lens-Family-Unchecked.html).

`mkLenses` merely generates the following definition
for each field, making use of Haskell's record update syntax:

    lensName f a = (\x -> a { fieldName = x }) `fmap` f (fieldName a)

`mkLenses` will refuse to create lenses for data declarations
with more than 1 constructor.
