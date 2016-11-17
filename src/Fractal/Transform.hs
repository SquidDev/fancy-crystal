module Fractal.Transform(translate, rotateX, rotateY, rotateZ, scale) where

import Data.Matrix

translate :: Num a => a -> a -> a -> Matrix a
translate x y z = fromList 4 4 [
  1, 0, 0, x,
  0, 1, 0, y,
  0, 0, 1, z,
  0, 0, 0, 1
  ]

rotateX :: Floating a => a -> Matrix a
rotateX a =
  fromList 4 4 [
    1, 0,  0, 0,
    0, c, -s, 0,
    0, s,  c, 0,
    0, 0,  0, 1
  ]
  where s = sin a
        c = cos a

rotateY :: Floating a => a -> Matrix a
rotateY a =
  fromList 4 4 [
    c, 0, -s, 0,
    0, 1,  0, 0,
    s, 0,  c, 0,
    0, 0,  0, 1
  ]
  where s = sin a
        c = cos a

rotateZ :: Floating a => a -> Matrix a
rotateZ a =
  fromList 4 4 [
     c, s, 0, 0,
    -s, c, 0, 0,
     0, 0, 1, 0,
     0, 0, 0, 1
  ]
  where s = sin a
        c = cos a

scale :: Num a => a -> a -> a -> Matrix a
scale x y z =
  fromList 4 4 [
    x, 0, 0, 0,
    0, y, 0, 0,
    0, 0, z, 0,
    0, 0, 0, 1
  ]
