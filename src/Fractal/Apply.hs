{-# LANGUAGE FlexibleContexts #-}

module Fractal.Apply(
  ColorHSV(ColorHSV), ApplyScope(), Tri(Tri), Transformer(), Vec3,
  baseScope,
  trans, hue, sat, bright, box,
  run, chooseList
) where

import Control.Monad.RWS
import Data.Functor
import Data.Matrix
import System.Random

data ColorHSV a = ColorHSV a a a deriving (Show, Eq, Ord)

type Vec3 a = (a, a, a)
data Tri a = Tri (Vec3 a) (Vec3 a) (Vec3 a) (ColorHSV a) deriving (Show, Eq, Ord)

data ApplyScope a = ApplyScope {
  depth :: Int,
  transform :: Matrix a,
  color :: ColorHSV a
  }

type Transformer a = RWS (ApplyScope a) [Tri a] StdGen

baseScope :: Num a => Int -> ColorHSV a -> ApplyScope a
baseScope d c = ApplyScope { depth = d, transform = identity 4, color = c }

box :: Num a => Transformer a ()
box = do
  (ApplyScope _ m c) <- ask
  let p0 = tri m 0 0 0
  let p1 = tri m 1 0 0
  let p2 = tri m 1 1 0
  let p3 = tri m 1 0 1
  let p4 = tri m 1 1 1
  let p5 = tri m 0 1 0
  let p6 = tri m 0 1 1
  let p7 = tri m 0 0 1
  tell [
           Tri p2 p0 p1 c, Tri p2 p5 p0 c, -- Front
           Tri p4 p1 p3 c, Tri p4 p2 p1 c, -- Right
           Tri p4 p5 p2 c, Tri p4 p6 p5 c, -- Top
           Tri p6 p3 p7 c, Tri p6 p4 p3 c, -- Back
           Tri p0 p7 p3 c, Tri p0 p3 p1 c, -- Bottom
           Tri p6 p7 p0 c, Tri p6 p0 p5 c  -- Left
       ]

  where tri m x y z =
          let p = fromList 4 1 [x, y, z, 1] in
          let p' = m * p in
          (p' ! (1, 1), p' ! (2, 1), p' ! (3, 1))

trans :: Num a => Matrix a -> ApplyScope a -> ApplyScope a
trans t (ApplyScope d m c) = ApplyScope { depth = d, transform = m * t, color = c }

hue :: Num a => a -> ApplyScope a -> ApplyScope a
hue h' (ApplyScope d m (ColorHSV h s b)) = ApplyScope { depth = d, transform = m, color = ColorHSV (h + h') s b }

sat :: (Num a, Ord a) => a -> ApplyScope a -> ApplyScope a
sat s' (ApplyScope d m (ColorHSV h s b)) = ApplyScope { depth = d, transform = m, color = ColorHSV h (clamp $ s * s') b }

bright :: (Num a, Ord a) => a -> ApplyScope a -> ApplyScope a
bright b' (ApplyScope d m (ColorHSV h s b)) = ApplyScope { depth = d, transform = m, color = ColorHSV h s (clamp $ b * b') }

run :: (ApplyScope a -> ApplyScope a) -> Transformer a r -> Transformer a r
run change action = do
  (ApplyScope d m c) <- ask
  if d < 0 then
    return undefined
  else
    local (const $ change (ApplyScope (d - 1) m c)) action

clamp :: (Ord a, Num a) => a -> a
clamp val | val > 1   = 1
          | val < 0   = 0
          | otherwise = val

chooseList :: [(r, Rational)] -> Transformer a r
chooseList [] = error "chooseList called with empty list"
chooseList [(x,_)] = return x
chooseList xs = do
  let s = fromRational (sum (map snd xs)) :: Double -- total weight
      cs = scanl1 (\(_,q) (y,s') -> (y, s'+q)) xs   -- cumulative weight
  p <- toRational <$> state (randomR (0.0,s))
  return . fst . head $ dropWhile (\(_,q) -> q < p) cs
