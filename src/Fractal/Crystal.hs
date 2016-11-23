module Fractal.Crystal where

import Control.Monad
import Fractal.Apply
import Fractal.Transform

main :: Transformer Double ()
main = join $ chooseList [(main1, 100),(main2, 100),(main3, 100), (tiny, 3)]
  where
        main1 :: Transformer Double ()
        main1 = do
          run id final
          run (trans (translate 0 0.4 0 * rotateX 89 * scale 0.995 0.995 0.995)                 . hue   1  . bright 0.997) main
        main2 :: Transformer Double ()
        main2 = do
          run id final
          run (trans (translate 0 0.4 0 * rotateX 89 * scale 0.995 0.995 0.995 * rotateZ   90)  . hue (-1) . bright 0.997) main
        main3 = do
          run id final
          run (trans (translate 0 0.4 0 * rotateX 89 * scale 0.995 0.995 0.995 * rotateZ (-90)) . hue    0 . bright 0.997) main
        tiny = do
          run (trans (rotateZ 15)) main
          run (trans (rotateY 4) . hue 1) main

final :: Transformer Double ()
final = join $ chooseList [
    (run (trans $ scale 1 3 1) box, 1),
    (run (trans $ scale 3 1 1) box, 1),
    (return (), 1)
  ]
