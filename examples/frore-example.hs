
import Data.IORef

import Graphics.UI.GLUT
import Graphics.Rendering.Frore


main :: IO ()
main = do
  -- initialize window
  _ <- getArgsAndInitialize
  initialDisplayMode $= [{-WithSamplesPerPixel 16,-}WithDepthBuffer,WithDepthBuffer,RGBAMode,WithAlphaComponent]
  _ <- createWindow "Frore example"

  -- load shader programs
  p1 <- loadShaders "examples/shader.vert" "examples/shader.frag"
  texID1 <- get $ uniformLocation p1 "tex"
  matPID1 <- findULoc "P" p1
  matVID1 <- findULoc "V" p1
  matMID1 <- findULoc "M" p1

  -- load shader programs
  p2 <- loadShaders "examples/shader.vert" "examples/simple.frag"
  texID2 <- get $ uniformLocation p2 "tex"
  matPID2 <- findULoc "P" p2
  matVID2 <- findULoc "V" p2
  matMID2 <- findULoc "M" p2

  projectionMat <- newIORef =<< orthographicMatrix (Size 0 0) (Size 100 100)
  viewMat <- newIORef =<< translationMatrix (Vector3 400 400 0)
  modelMat <- newIORef =<< rotationMatrix (Vector3 0 0 (-1)) 0

  font <- makeFont "/usr/share/fonts/TTF/DejaVuSans.ttf"
  (tex1, tex2) <- renderText font 128 30 64 "ж"

  -- it is needed for rendering
  clientState VertexArray $= Enabled
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  texture Texture2D $= Enabled
  clearColor $= Color4 1 1 1 1

  [vao] <- genObjectNames 1
  bindVertexArrayObject $= Just vao

  s <- square
  suv <- squareUV

  reshapeCallback $= (Just $ \s@(Size w h) -> do
    viewport $= (Position 0 0, s)
    pm <- orthographicMatrix (Size 0 0) s
    projectionMat $= pm
    vm <- translationMatrix (Vector3 (fromIntegral . div w $ 2) (fromIntegral . div h $ 2) 0)
    viewMat $= vm
    return ())

  displayCallback $= do
    clear [ColorBuffer]

    bindVao 3 0 s
    bindVao 2 1 suv

    uniform texID1 $= TextureUnit 0
    curMat <- get projectionMat
    placeMatrix curMat matPID1
    curMat <- get viewMat
    placeMatrix curMat matVID1
    curMat <- get modelMat
    placeMatrix curMat matMID1

    textureBinding Texture2D $= tex1
    currentProgram $= Just p1
    drawArrays Triangles 0 6


    uniform texID2 $= TextureUnit 0
    curMat <- get projectionMat
    placeMatrix curMat matPID2
    curMat <- get viewMat
    placeMatrix curMat matVID2
    curMat <- get modelMat
    placeMatrix curMat matMID2

    textureBinding Texture2D $= tex2
    currentProgram $= Just p2
    drawArrays Triangles 0 6

    swapBuffers
  mainLoop

orthographicMatrix :: Size -> Size -> IO(GLmatrix GLfloat)
orthographicMatrix (Size l b) (Size r t) =
  newMatrix ColumnMajor [ 2.0/(right-left),                 0,                 0, 0
                        ,                0,  2.0/(top-bottom),                 0, 0
                        ,                0,                 0, -2.0/(zfar-znear), 0
                        , -(right+left)/(right-left), -(top+bottom)/(top-bottom), -(zfar+znear)/(zfar-znear), 1]
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

rotationMatrix :: Vector3 GLfloat -> GLfloat -> IO(GLmatrix GLfloat)
rotationMatrix axis angle =
  newMatrix ColumnMajor [ oc * x * x + c    , oc * x * y - z * s, oc * z * x + y * s, 0.0
                        , oc * x * y + z * s, oc * y * y + c    , oc * y * z - x * s, 0.0
                        , oc * z * x - y * s, oc * y * z + x * s, oc * z * z + c    , 0.0
                        , 0.0               , 0.0               , 0.0               , 1.0]
 where Vector3 x y z = normalizeVector3 axis
       s  = sin angle
       c  = cos angle
       oc = 1 - c

translationMatrix :: Vector3 GLfloat -> IO(GLmatrix GLfloat)
translationMatrix (Vector3 x y z) =
  newMatrix ColumnMajor [ 1, 0, 0, 0
                        , 0, 1, 0, 0
                        , 0, 0, 1, 0
                        , x, y, z, 1]

normalizeVector3 :: Vector3 GLfloat -> Vector3 GLfloat
normalizeVector3 (Vector3 x y z) = Vector3 (x/l) (y/l) (z/l)
 where l = sqrt $ x*x + y*y + z*z
