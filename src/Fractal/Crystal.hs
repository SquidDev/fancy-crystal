module Fractal.Crystal where

import Fractal.Apply
import Fractal.Transform
import Control.Monad.Random
import System.Random

main :: Num a => RandT StdGen (Transformer a) ()
main = fromList [(undefined, 100)]
  where
        main1 = lift getRandom $ run final
{-          run $ do
            trans $ translate 0 0.4 0
            trans $ rotateX 89
            trans $ scale 0.995 0.995 0.995
            bright 0.999
            main-}

final :: Num a => RandT StdGen (Transformer a) ()
final = undefined
