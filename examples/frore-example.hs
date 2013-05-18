
import Data.IORef

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.Frore


main :: IO ()
main = do
  -- initialize window
  _ <- getArgsAndInitialize
  initialDisplayMode $= [WithSamplesPerPixel 16,WithDepthBuffer,WithDepthBuffer,RGBAMode,WithAlphaComponent]
  _ <- createWindow "Frore example"

   -- load shader programs
  p <- loadShaders "examples/shader.vert" "examples/shader.frag"
  texID <- get $ uniformLocation p "tex"
  matPID <- findULoc "P" p
  matVID <- findULoc "V" p
  matMID <- findULoc "M" p

  projectionMat <- newIORef =<< orthographicMatrix (Size 0 0) (Size 100 100)
  viewMat <- newIORef =<< identityMatrix
  modelMat <- newIORef =<< identityMatrix

  -- it is needed for rendering
  clientState VertexArray $= Enabled
  clearColor $= Color4 0.1 0.1 0.5 1
  currentProgram $= Just p

  [vao] <- genObjectNames 1
  bindVertexArrayObject $= Just vao

  s <- square
  suv <- squareUV

  
  reshapeCallback $= (Just $ \s -> do
    viewport $= (Position 0 0, s)
    m <- perspectivePixelPerfectMatrix s 1500 3 (-3) -- orthographicMatrix (Size 0 0) s
    projectionMat $= m
    return ())

  displayCallback $= do
    clear [ColorBuffer]

    -- textureBinding Texture2D $= (textures !! curImageNo)

    uniform texID $= TextureUnit 0

    curMat <- get projectionMat
    withMatrix curMat $ \_ ptr -> glUniformMatrix4fv matPID 1 0 ptr
    curMat <- get viewMat
    withMatrix curMat $ \_ ptr -> glUniformMatrix4fv matVID 1 0 ptr
    curMat <- get modelMat
    withMatrix curMat $ \_ ptr -> glUniformMatrix4fv matMID 1 0 ptr

    bindVao 3 0 s
    bindVao 2 1 suv
    drawArrays Triangles 0 6
    swapBuffers
  mainLoop

orthographicMatrix :: Size -> Size -> IO(GLmatrix GLfloat)
orthographicMatrix (Size l b) (Size r t) =
  newMatrix ColumnMajor [ 2.0/(right-left),                 0,                 0, -(right+left)/(right-left)
                        ,                0,  2.0/(top-bottom),                 0, -(top+bottom)/(top-bottom)
                        ,                0,                 0, -2.0/(zfar-znear), -(zfar+znear)/(zfar-znear)
                        ,                0,                 0,                 0,                          1]
 where zfar   = -1
       znear  =  1
       left   = fromIntegral l
       right  = fromIntegral r
       top    = fromIntegral t
       bottom = fromIntegral b

identityMatrix :: IO(GLmatrix GLfloat)
identityMatrix =
  newMatrix ColumnMajor [ 1, 0, 0, 0
                        , 0, 1, 0, 0
                        , 0, 0, 1, 0
                        , 0, 0, 0, 1]

-- | width and height defines the 2D space available at z=0, must be the same
--   as the size of the viewport.
--   z_near defines the z position of the near plane, must be greater than 0.
--   z_far defines the z position of the far plane, must be lesser than 0.
--   z_eye defines the position of the viewer, must be greater that z_near.
perspectivePixelPerfectMatrix :: Size -> GLfloat -> GLfloat -> GLfloat -> IO(GLmatrix GLfloat)
perspectivePixelPerfectMatrix (Size w h) z_eye z_near z_far =
  newMatrix ColumnMajor [(2 * z_eye) / width, 0, 0, 0
                        , 0, (2 * z_eye) / height, 0, 0
                        , 0, 0, ktz - ksz * z_eye, -1
                        , 0 :: GLfloat, 0, ksz, z_eye]
 where kdn = z_eye - z_near
       kdf = z_eye - z_far
       ksz = - (kdf + kdn) / (kdf - kdn)
       ktz = - (2 * kdn * kdf) / (kdf - kdn)
       width = 2 * fromIntegral (w `div` 2)
       height = 2 * fromIntegral (h `div` 2)

