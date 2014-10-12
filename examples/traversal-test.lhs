To see the results of these ghci interactions on your own machine, run:

    [bash]
    BlogLiterately -g examples/traversal-test.lhs > test.html && firefox test.html

> {-# LANGUAGE TemplateHaskell #-}

> import Lens.Family2
> import Lens.Family2.TH

> data Opt b c d = A | B b | C c d Int
>                deriving (Eq, Show, Read, Ord)
> $(makeTraversals ''Opt)

   [ghci]
   _B %~ (+1) $ A 3
   _B %~ (+1) $ B 3
   _B %~ (+1) $ C 3
