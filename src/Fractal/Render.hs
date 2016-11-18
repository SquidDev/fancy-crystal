module Fractal.Render(display) where

import Fractal.Apply
import Graphics.UI.GLUT
import Data.Fixed
import Data.IORef
import Control.Monad

display :: [Tri Double] -> IO ()
display points = do
  (_, _) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  _ <- createWindow "Main"
  depthFunc $= Just Less

  angle <- newIORef (0 :: GLfloat)
  size <- newIORef (1 :: GLfloat)
  pos <- newIORef (0, 0)

  displayCallback $= do
    (x', y') <- get pos
    angle' <- get angle
    size' <- get size
    loadIdentity
    preservingMatrix $ do
      translate $ Vector3 x' y' 0
      rotate angle' $ Vector3 0 1 0
      scale size' size' size'
      draw points
    swapBuffers
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse angle pos size)
  idleCallback $= Just (postRedisplay Nothing)
  mainLoop

draw :: [Tri Double] -> DisplayCallback
draw points = do
  clear [ColorBuffer, DepthBuffer]
  preservingMatrix $ do
    scale (0.1 :: GLfloat) 0.1 0.1
    renderPrimitive Quads $ mapM_ (\(Tri p1 p2 p3 c) -> col c >> point p1 >> point p2 >> point p3) points
  flush
    where point :: Vec3 Double -> IO ()
          point (x, y, z) = vertex $ Vertex3 x y z

          -- TODO: Inline this
          col :: ColorHSV Double -> IO ()
          col (ColorHSV h s v) =
            color $ case hi of
                      0 -> Color3 v t p
                      1 -> Color3 q v p
                      2 -> Color3 p v t
                      3 -> Color3 p q v
                      4 -> Color3 t p v
                      5 -> Color3 v p q
            where
              hi = floor (h / 60) `mod` 6
              f = (h / 60) `mod'` 1
              p = v * (1 - s)
              q = v * (1 - f * s)
              t = v * (1 - (1 - f) * s)

reshape :: ReshapeCallback
reshape size = viewport $= (Position 0 0, size)

keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IORef GLfloat -> KeyboardMouseCallback
keyboardMouse angle pos size key Down _ _ = case key of
                                              (Char '+') -> size $~! (* 1.1)
                                              (Char '-') -> size $~! (/ 1.1)
                                              (Char 'a') -> angle $~! (+ 20)
                                              (Char 'd') -> angle $~! (\x -> x - 20)
                                              (SpecialKey KeyLeft) -> pos $~! \(x, y) -> (x - 0.1, y)
                                              (SpecialKey KeyRight) -> pos $~! \(x, y) -> (x + 0.1, y)
                                              (SpecialKey KeyDown) -> pos $~! \(x, y) -> (x, y - 0.1)
                                              (SpecialKey KeyUp) -> pos $~! \(x, y) -> (x, y + 0.1)
                                              _ -> return ()
keyboardMouse _ _ _ _ _ _ _ = return ()
