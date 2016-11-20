module Main where

import Fractal.Apply
import Fractal.Render
import Fractal.Transform
import qualified Fractal.Crystal as Crystal
import System.Random
import Control.Monad.RWS

-- TODO: Ambient occlusion from http://john-chapman-graphics.blogspot.co.uk/2013/01/ssao-tutorial.html
-- TODO: Fancy colours, maybe https://en.wikipedia.org/wiki/Iridescence
-- TODO: Correct normals: they should be the average of all three points

main :: IO ()
main = do
  r <- getStdGen
  let points = snd $ execRWS Crystal.main (baseScope 600 (ColorHSV 0 1 1)) r
  let points' = snd $ execRWS box (baseScope 600 (ColorHSV 0 0 1)) r
  let mapped' = snd $ execRWS (do
                                  box
                                  run (trans (scale 5 0.5 5 * translate (-0.5) (-2) (-0.5))) box
                              ) (baseScope 600 (ColorHSV 0 0 1)) r
  display points
