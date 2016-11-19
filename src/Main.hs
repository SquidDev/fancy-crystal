module Main where

import Fractal.Apply
import Fractal.Render
import qualified Fractal.Crystal as Crystal
import System.Random
import Control.Monad.RWS

main :: IO ()
main = do
  r <- getStdGen
  let points = snd $ execRWS Crystal.main (baseScope 600 (ColorHSV 0 0 1)) r
  let points' = snd $ execRWS box (baseScope 600 (ColorHSV 0 0 1)) r
  display points
