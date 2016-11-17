{-# LANGUAGE FlexibleContexts #-}

module Fractal.Apply(
  ColorHSV, ApplyScope(), Box(), Transformer(),
  baseScope,
  trans, hue, sat, bright, box,
  run, runTransformer
) where

import Data.Matrix
import Fractal.Scope
import Control.Monad.Writer
import Control.Monad.State

data ColorHSV a = ColorHSV a a a
data Box a = Box a a a (ColorHSV a)

data ApplyScope a = ApplyScope {
  depth :: Int,
  transform :: Matrix a,
  color :: ColorHSV a
  }

data Transformer a r = Transformer (WriterT [Box a] (State (ApplyScope a)) r)

baseScope :: Num a => Int -> ColorHSV a -> ApplyScope a
baseScope d c = ApplyScope { depth = d, transform = identity 4, color = c }

box :: Num a => a -> a -> a -> Transformer a ()
box x y z = Transformer $ do
  (ApplyScope _ m c) <- getScope
  let mat = fromList 4 1 [x, y, z, 1]
  let t' = m * mat
  tell [Box (t' ! (0, 0)) (t' ! (1, 0)) (t' ! (2, 0)) c]

trans :: Num a => Matrix a -> Transformer a ()
trans t = Transformer $ do
  (ApplyScope d m c) <- getScope
  putScope ApplyScope { depth = d, transform = m * t, color = c }

hue :: Num a => a -> Transformer a ()
hue h' = Transformer $ do
  (ApplyScope d m (ColorHSV h s b)) <- getScope
  putScope ApplyScope { depth = d, transform = m, color = ColorHSV (h + h') s b }

sat :: (Num a, Ord a) => a -> Transformer a ()
sat s' = Transformer $ do
  (ApplyScope d m (ColorHSV h s b)) <- getScope
  putScope ApplyScope { depth = d, transform = m, color = ColorHSV h (clamp $ s * s') b }

bright :: (Num a, Ord a, MonadScope (ApplyScope a) m) => a -> m ()
bright b' = do
  (ApplyScope d m (ColorHSV h s b)) <- getScope
  putScope ApplyScope { depth = d, transform = m, color = ColorHSV h s (clamp $ b * b') }

run :: Transformer a r -> Transformer a r
run (Transformer action) = Transformer $ do
  (ApplyScope d m c) <- getScope
  if d < 0 then
    return undefined
  else
    localScope (const $ ApplyScope (d - 1) m c) action

runTransformer :: ApplyScope a -> Transformer a r -> [Box a]
runTransformer initial (Transformer action) = evalState (execWriterT action) initial

clamp :: (Ord a, Num a) => a -> a
clamp val | val > 1   = 1
          | val < 0   = 0
          | otherwise = val
