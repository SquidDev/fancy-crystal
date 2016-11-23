module Main where

import Control.Monad.RWS
import Fractal.Apply
import Fractal.Render
import Fractal.Transform
import qualified Fractal.Crystal as Crystal
import System.Environment
import System.Random

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
