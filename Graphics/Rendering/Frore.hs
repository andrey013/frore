module Graphics.Rendering.Frore where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Graphics.Rendering.OpenGL.Raw.Core31

import qualified Data.Vector.Storable as V

import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr

import System.IO

-- big square
square :: IO BufferObject
square = initGeometry . V.fromList $ map (*100) $
  concat [[-1, -1, 0],[ 1, -1, 0],[ 1,  1, 0]
         ,[ 1,  1, 0],[-1,  1, 0],[-1, -1, 0]]

-- texture coordinates
squareUV :: IO BufferObject
squareUV = initGeometry . V.fromList $
  concat [[ 0, 0],[ 1, 0],[ 1, 1]
         ,[ 1, 1],[ 0, 1],[ 0, 0]]

initGeometry :: V.Vector GLfloat -> IO BufferObject
initGeometry tris = do
  [vbo] <- genObjectNames 1
  bindBuffer ArrayBuffer $= Just vbo
  let len = fromIntegral $ V.length tris * sizeOf (V.head tris)
  V.unsafeWith tris $ \ptr ->
    bufferData ArrayBuffer $= (len, ptr, StaticDraw)
  return vbo

bindVao :: GLint -> GLuint -> BufferObject -> IO ()
bindVao size loc vb = do
  vertexAttribArray (AttribLocation loc) $= Enabled
  bindBuffer ArrayBuffer $= Just vb
  vertexAttribPointer (AttribLocation loc) $= (ToFloat, VertexArrayDescriptor size Float 0 nullPtr)

initProgram :: String -> String -> IO Program
initProgram v f = do
  [vSh] <- genObjectNames 1
  [fSh] <- genObjectNames 1
  shaderSource vSh $= [v]
  shaderSource fSh $= [f]
  compileShader vSh
  compileShader fSh
  [shProg] <- genObjectNames 1
  attachedShaders shProg $= ([vSh], [fSh])
  linkProgram shProg
  print =<< get (shaderInfoLog vSh)
  print =<< get (shaderInfoLog fSh)
  print =<< get (programInfoLog shProg)
  return shProg

-- | Load a vertex shader and a fragment shader from the specified files
loadShaders :: String -> String -> IO Program
loadShaders vFile fFile =
  withFile vFile ReadMode $ \vHandle ->
    withFile fFile ReadMode $ \fHandle -> do
      vText <- hGetContents vHandle
      fText <- hGetContents fHandle
      initProgram vText fText

findULoc :: String -> Program -> IO GLint
findULoc s p = withCString s (\c_string -> let gl_string = castPtr c_string in glGetUniformLocation (programID p) gl_string)