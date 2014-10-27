To verify the results of these ghci interactions on your own machine, run:

    [bash]
    BlogLiterately -g examples/traversal-test.lhs > test.html && firefox test.html

(Make sure you have the lens-family package installed.)

> {-# LANGUAGE TemplateHaskell #-}

> import Lens.Family2
> import Lens.Family2.TH

> data Opt b c d = A | B b | CD c d Int
>                deriving (Eq, Show, Read, Ord)
> $(makeTraversals ''Opt)

    [ghci]
    _B %~ (+1) $ A
      A
    _B %~ (+1) $ B 3
      B 4
    _B %~ (+1) $ CD 3 4 5
      CD 3 4 5
