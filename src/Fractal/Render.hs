module Fractal.Render(display, Options(..)) where

import Control.Monad
import Data.Fixed
import Data.IORef
import Foreign.Storable (sizeOf)
import Fractal.Apply
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.GLUT (($~!))
import qualified Control.Lens as L(view, over)
import qualified Graphics.GLUtil as U
import qualified Graphics.GLUtil.Camera3D as Camera3D
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import qualified Linear as L

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

data Options = Options { debug :: Bool, animate :: Bool }

display :: Options -> [Tri Double] -> IO ()
display options points = do
  (_, _) <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [GLUT.WithDepthBuffer, GLUT.DoubleBuffered]
  _ <- GLUT.createWindow "Main"
  GLUT.windowSize $= GL.Size 800 600
  GL.depthFunc $= Just GL.Less
  GL.cullFace $= Just GL.Front

  light <- newIORef $ L.V3 0 5 5
  pos <- newIORef $ L.V3 0 0 (-12)
  rotation <- newIORef (L.axisAngle 0 0 :: L.Quaternion Float)

  shaders <- U.loadShaderProgram [(GL.VertexShader, "shaders/main.v.glsl"), (GL.FragmentShader, "shaders/main.f.glsl")]
  shadowShader <- U.loadShaderProgram [(GL.VertexShader, "shaders/shadow.v.glsl"), (GL.FragmentShader, "shaders/shadow.f.glsl")]

  -- Render the depth buffer
  depthBuffer <- GL.genObjectName
  GL.bindFramebuffer GL.Framebuffer $= depthBuffer

  depthTexture <- GL.genObjectName
  GL.textureBinding GL.Texture2D $= Just depthTexture
  GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.DepthComponent16 (GL.TextureSize2D 1024 1024) 0 (GL.PixelData GL.DepthComponent GL.Float U.offset0)
  GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)

  GL.framebufferTexture2D GL.Framebuffer GL.DepthAttachment GL.Texture2D depthTexture 0
  GL.drawBuffer $= GL.NoBuffers

  points' <- initBuffer points
  let pointSize = fromIntegral $ length points * 3

  GLUT.displayCallback $= do
    -- Some basic camera work
    light' <- GLUT.get light
    pos' <- GLUT.get pos
    rotation' <- GLUT.get rotation

    size <- GLUT.get GLUT.windowSize
    let (GLUT.Size width height) = size

    let model = L.mkTransformation rotation' (L.V3 0 0 0) -- L.identity

    -- Render the shadow map
    GL.bindFramebuffer GL.Framebuffer $= depthBuffer
    GL.viewport $= (GL.Position 0 0, GL.Size 1024 1024)
    GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]

    GL.currentProgram $= Just (U.program shadowShader)
    U.enableAttrib shadowShader "position"

    -- Render the depth buffer
    let depthProj = Camera3D.orthoMatrix (-10) 10 (-10) 10 (-10) 20
    let depthView = (L.lookAt light' (L.V3 0 0 0) (L.V3 0 1 0) :: L.M44 Float)
    let depthMVP = depthProj L.!*! depthView L.!*! model

    U.setUniform shadowShader "mvp" depthMVP
    U.setAttrib shadowShader "position" GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Double (floatSize * 9) U.offset0
    GL.bindBuffer GL.ArrayBuffer $= Just points'

    GL.drawArrays GL.Triangles 0 pointSize

    GL.vertexAttribArray (U.getAttrib shaders "position") $= GL.Disabled

    -- Render the main screen
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

    if debug options then
      GL.viewport $= (GL.Position 0 0, GLUT.Size (width `div` 2) (height `div` 2))
    else
      GL.viewport $= (GL.Position 0 0, GLUT.Size width height)

    GL.clearColor $= GL.Color4 1 1 1 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    GL.currentProgram $= Just (U.program shaders)
    U.enableAttrib shaders "position"
    U.enableAttrib shaders "normal"
    U.enableAttrib shaders "color"

    let bias = L.V4 (L.V4 0.5 0 0 0.5) (L.V4 0 0.5 0 0.5) (L.V4 0 0 0.5 0.5) (L.V4 0 0 0 1)
    let projection = Camera3D.projectionMatrix (pi/4) (fromIntegral width / fromIntegral height) 0.1 100 :: L.M44 GL.GLfloat
    let view = L.mkTransformation 1 pos' -- L.!*! L.mkTransformation rotation' (L.V3 0 0 0)
    let viewModel = view L.!*! model
    U.setUniform shaders "mvp" (projection L.!*! viewModel)
    U.setUniform shaders "mv_inv" (L.transpose $ L.inv33 $ L.view L._m33 viewModel)
    U.setUniform shaders "mv" viewModel
    U.setUniform shaders "v" view
    U.setUniform shaders "mvp_bias" (bias L.!*! depthMVP)
    U.setUniform shaders "lightPosition" light'

    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just depthTexture
    U.setUniform shaders "shadow" $ GL.TextureUnit 0

    GL.bindBuffer GL.ArrayBuffer $= Just points'

    U.setAttrib shaders "position" GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Double (floatSize * 9) U.offset0
    U.setAttrib shaders "normal" GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Double (floatSize * 9) (U.offsetPtr (floatSize * 3))
    U.setAttrib shaders "color" GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Double (floatSize * 9) (U.offsetPtr (floatSize * 6))

    GL.drawArrays GL.Triangles 0 pointSize

    GL.vertexAttribArray (U.getAttrib shaders "position") $= GL.Disabled
    GL.vertexAttribArray (U.getAttrib shaders "normal") $= GL.Disabled
    GL.vertexAttribArray (U.getAttrib shaders "color") $= GL.Disabled

    GL.currentProgram $= Nothing
    GL.cullFace $= Nothing

    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just depthTexture
    GL.texture GL.Texture2D $= GL.Enabled

    when (debug options) $ do
      GL.viewport $= (GL.Position (width `div` 2) 0, GLUT.Size (width `div` 2) (height `div` 2))
      GL.preservingMatrix $ do
        GL.loadIdentity
        GL.renderPrimitive GL.Quads $ do
          GL.texCoord (GL.TexCoord2 (0 :: GL.GLfloat) 0)
          GL.vertex (GL.Vertex2 (-1 :: GL.GLfloat) (-1))

          GL.texCoord (GL.TexCoord2 (0 :: GL.GLfloat) 1)
          GL.vertex (GL.Vertex2 (-1 :: GL.GLfloat) 1)

          GL.texCoord (GL.TexCoord2 (1 :: GL.GLfloat) 1)
          GL.vertex (GL.Vertex2 (1 :: GL.GLfloat) 1)

          GL.texCoord (GL.TexCoord2 (1 :: GL.GLfloat) 0)
          GL.vertex (GL.Vertex2 (1 :: GL.GLfloat) (-1))
    GLUT.swapBuffers
  GLUT.reshapeCallback $= Just reshape
  GLUT.keyboardMouseCallback $= Just (keyboardMouse light pos rotation)
  GLUT.idleCallback $= Just (do
    when (animate options) (rotation $~! (L.axisAngle (L.V3 0 1 0) (-pi/1000) *))
    GLUT.postRedisplay Nothing)
  GLUT.mainLoop

  GL.deleteObjectName points'
  GL.deleteObjectName depthBuffer
  GL.deleteObjectName depthTexture

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

dP :: Float
dP = 0.1

dA :: Float
dA = pi/40

keyboardMouse :: IORef (L.V3 Float) -> IORef (L.V3 Float) -> IORef (L.Quaternion Float) -> GLUT.KeyboardMouseCallback
keyboardMouse light pos rot key GLUT.Down _ _ = case key of
                                                  (GLUT.Char 'w') -> rot $~! (L.axisAngle (L.V3 1 0 0) (-dA) *)
                                                  (GLUT.Char 's') -> rot $~! (L.axisAngle (L.V3 1 0 0) dA *)
                                                  (GLUT.Char 'a') -> rot $~! (L.axisAngle (L.V3 0 1 0) dA *)
                                                  (GLUT.Char 'd') -> rot $~! (L.axisAngle (L.V3 0 1 0) (-dA) *)

                                                  (GLUT.Char 'u') -> light $~! L.over L._z (subtract dP)
                                                  (GLUT.Char 'o') -> light $~! L.over L._z (+dP)
                                                  (GLUT.Char 'j') -> light $~! L.over L._x (subtract dP)
                                                  (GLUT.Char 'l') -> light $~! L.over L._x (+dP)
                                                  (GLUT.Char 'k') -> light $~! L.over L._y (subtract dP)
                                                  (GLUT.Char 'i') -> light $~! L.over L._y (+dP)

                                                  (GLUT.Char 'p') -> do
                                                    light' <- GLUT.get light
                                                    pos' <- GLUT.get pos
                                                    rot' <- GLUT.get rot
                                                    print (light', pos', rot')

                                                  (GLUT.Char '-') -> pos $~! L.over L._z (subtract dP)
                                                  (GLUT.Char '+') -> pos $~! L.over L._z (+dP)
                                                  (GLUT.SpecialKey GLUT.KeyLeft) -> pos $~! L.over L._x (subtract dP)
                                                  (GLUT.SpecialKey GLUT.KeyRight) -> pos $~! L.over L._x (+dP)
                                                  (GLUT.SpecialKey GLUT.KeyDown) -> pos $~! L.over L._y (subtract dP)
                                                  (GLUT.SpecialKey GLUT.KeyUp) -> pos $~! L.over L._y (+dP)
                                                  _ -> return ()
keyboardMouse _ _ _ _ _ _ _ = return ()

floatSize :: Num a => a
floatSize = fromIntegral $ sizeOf (undefined :: GL.GLdouble)
