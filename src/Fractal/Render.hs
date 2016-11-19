module Fractal.Render(display) where

import Fractal.Apply
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.GLUT (($~!))
import Data.Fixed
import Data.IORef
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as Camera3D
import Foreign.Storable (sizeOf)
import qualified Linear as L
import qualified Control.Lens as L(view)

initShaders :: IO U.ShaderProgram
initShaders = U.loadShaderProgram [(GL.VertexShader, "main.v.glsl"), (GL.FragmentShader, "main.f.glsl")]

initBuffer :: [Tri Double] -> IO GL.BufferObject
initBuffer points = U.makeBuffer GL.ArrayBuffer $ concatMap handle points
  where handle :: Tri Double -> [GL.GLdouble]
        handle (Tri (x1, y1, z1) (x2, y2, z2) (x3, y3, z3) col) =
          let (r, g, b) = hsvToRgb col in
          let a1 = L.V3 x1 y1 z1 in
          let a2 = L.V3 x2 y2 z2 in
          let a3 = L.V3 x3 y3 z3 in
          let (L.V3 n1 n2 n3) = L.cross (a3 - a2) (a1 - a2) in
            [
              x1, y1, z1, n1, n2, n3, r, g, b,
              x2, y2, z2, n1, n2, n3, r, g, b,
              x3, y3, z3, n1, n2, n3, r, g, b
            ]

display :: [Tri Double] -> IO ()
display points = do
  (_, _) <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [GLUT.WithDepthBuffer, GLUT.DoubleBuffered]
  _ <- GLUT.createWindow "Main"
  GL.depthFunc $= Just GL.Less
  GL.cullFace $= Just GL.Front

  camera <- newIORef $ Camera3D.dolly (L.V3 0 0 (-4)) Camera3D.fpsCamera

  shaders <- initShaders
  points' <- initBuffer points
  let pointSize = fromIntegral $ length points * 3

  GLUT.displayCallback $= do
    camera' <- GLUT.get camera

    GL.clearColor $= GL.Color4 0.7 0.7 0.7 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    size <- GLUT.get GLUT.windowSize
    GL.viewport $= (GL.Position 0 0, size)

    let (GLUT.Size width height) = size
    GL.currentProgram $= Just (U.program shaders)
    U.enableAttrib shaders "position"
    U.enableAttrib shaders "normal"
    U.enableAttrib shaders "color"

    let projection = Camera3D.projectionMatrix (pi/4) (fromIntegral width / fromIntegral height) 0.1 100 :: L.M44 GL.GLfloat
    let view = camMatrix camera'
    U.asUniform (projection L.!*! view) (U.getUniform shaders "mvp")
    U.asUniform (L.transpose $ L.inv33 $ L.view L._m33 view) (U.getUniform shaders "mv_inv")

    GL.bindBuffer GL.ArrayBuffer $= Just points'

    U.setAttrib shaders "position" GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Double (floatSize * 9) U.offset0
    U.setAttrib shaders "normal" GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Double (floatSize * 9) (U.offsetPtr (floatSize * 3))
    U.setAttrib shaders "color" GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Double (floatSize * 9) (U.offsetPtr (floatSize * 6))

    GL.drawArrays GL.Triangles 0 pointSize

    GL.vertexAttribArray (U.getAttrib shaders "position") $= GL.Disabled
    GL.vertexAttribArray (U.getAttrib shaders "normal") $= GL.Disabled
    GL.vertexAttribArray (U.getAttrib shaders "color") $= GL.Disabled

    GLUT.swapBuffers
  GLUT.reshapeCallback $= Just reshape
  GLUT.keyboardMouseCallback $= Just (keyboardMouse camera)
  GLUT.idleCallback $= Just (GLUT.postRedisplay Nothing)
  GLUT.mainLoop

hsvToRgb :: ColorHSV Double -> (Double, Double, Double)
hsvToRgb (ColorHSV h s v) =
  case hi of
    0 -> (v, t, p)
    1 -> (q, v, p)
    2 -> (p, v, t)
    3 -> (p, q, v)
    4 -> (t, p, v)
    5 -> (v, p, q)
    _ -> undefined
  where
    hi = floor (h / 60) `mod` 6 :: Int
    f = (h / 60) `mod'` 1
    p = v * (1 - s)
    q = v * (1 - f * s)
    t = v * (1 - (1 - f) * s)

reshape :: GLUT.ReshapeCallback
reshape size = GL.viewport $= (GL.Position 0 0, size)

keyboardMouse :: IORef (Camera3D.Camera GL.GLfloat) -> GLUT.KeyboardMouseCallback
keyboardMouse camera key GLUT.Down _ _ = case key of
                                              (GLUT.Char '+') -> camera $~! Camera3D.dolly (L.V3 0 0 (-0.1))
                                              (GLUT.Char '-') -> camera $~! Camera3D.dolly (L.V3 0 0 0.1)
                                              (GLUT.Char 'w') -> camera $~! Camera3D.tilt (-5)
                                              (GLUT.Char 's') -> camera $~! Camera3D.tilt 5
                                              (GLUT.Char 'a') -> camera $~! Camera3D.pan 5
                                              (GLUT.Char 'd') -> camera $~! Camera3D.pan (-5)
                                              (GLUT.SpecialKey GLUT.KeyLeft) -> camera $~! Camera3D.dolly (L.V3 (-0.1) 0 0)
                                              (GLUT.SpecialKey GLUT.KeyRight) -> camera $~! Camera3D.dolly (L.V3 0.1 0 0)
                                              (GLUT.SpecialKey GLUT.KeyDown) -> camera $~! Camera3D.dolly (L.V3 0 (-0.1) 0)
                                              (GLUT.SpecialKey GLUT.KeyUp) -> camera $~! Camera3D.dolly (L.V3 0 0.1 0)
                                              _ -> return ()
keyboardMouse _ _ _ _ _ = return ()

floatSize :: Num a => a
floatSize = fromIntegral $ sizeOf (undefined :: GL.GLdouble)

camMatrix :: (L.Conjugate a, L.Epsilon a, RealFloat a) => Camera3D.Camera a -> L.M44 a
camMatrix c = L.mkTransformation q (Camera3D.location c)
  where q = L.conjugate $ Camera3D.orientation c
