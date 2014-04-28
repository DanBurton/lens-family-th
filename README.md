lens-family-th
==============

Template Haskell to generate lenses for lens-family and lens-family-core.

Usage:

    {-# LANGUAGE TemplateHaskell, Rank2Types #-}

    import Lens.Family2
    import Lens.Family2.TH

    data Foo a = Foo { _bar :: Int, _baz :: a }
             deriving (Show, Read, Eq, Ord)
    $(makeLenses ''Foo)

This will create lenses `bar` and `baz`.

You can instead create these lenses by hand
as explained by documentation at [Lens.Family.Unchecked](http://hackage.haskell.org/packages/archive/lens-family-core/latest/doc/html/Lens-Family-Unchecked.html).

`makeLenses` merely generates the following definition
for each field, making use of Haskell's record update syntax:

    lensName f a = (\x -> a { fieldName = x }) `fmap` f (fieldName a)

`makeLenses` will refuse to create lenses for data declarations
with more than 1 constructor.

----

For data types with multiple constructors and one field each,
you can use `makeTraversals`. For example:

    {-# LANGUAGE TemplateHaskell, Rank2Types #-}

    import Lens.Family2
    import Lens.Family2.TH

    data T a b c = A a | B b | C c
    $(makeTraversals ''T)
    
Will create traversals `_A`, `_B`, and `_C` in this fashion:

    _A k (A a) = fmap A (k a)
    _A _  t    = pure t

Repeated for each constructor.
