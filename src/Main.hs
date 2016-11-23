module Main where

import Fractal.Apply
import Fractal.Render
import Fractal.Transform
import qualified Fractal.Crystal as Crystal
import System.Random
import System.Environment
import Control.Monad.RWS

-- TODO: Ambient occlusion from http://john-chapman-graphics.blogspot.co.uk/2013/01/ssao-tutorial.html
-- TODO: Fancy colours, maybe https://en.wikipedia.org/wiki/Iridescence
-- TODO: Correct normals: they should be the average of all three points

main :: IO ()
main = do
  args <- getArgs
  let options = Options {
        debug = "--debug" `elem` args || "-d" `elem` args,
        animate = not ("--static" `elem` args || "-s" `elem` args)
        }
  r <- getStdGen
  let points = snd $ execRWS Crystal.main (baseScope 600 (ColorHSV 275 1 1)) r
  display options points
