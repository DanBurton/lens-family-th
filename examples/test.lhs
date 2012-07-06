To see the results of these ghci interactions on your own machine, run:

    [bash]
    BlogLiterately -g examples/test.lhs > test.html && firefox test.html

> {-# LANGUAGE TemplateHaskell #-}

> import Lens.Family2
> import Lens.Family2.TH

> data Pair a b = Pair { _pairL :: a, _pairR :: b }
>               deriving (Eq, Show, Read, Ord)
> $(mkLenses ''Pair)

    [ghci]
    let p = Pair '1' 1
    p ^. pairL
    p ^. pairR
    :m +Data.Char
    (pairL ^%= digitToInt) p
    (pairR ^%= intToDigit) p
    (pairL ^= "foo") p
    (pairR ^= "bar") p
