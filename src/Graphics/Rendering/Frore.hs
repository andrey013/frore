module Graphics.Rendering.Frore (
    square
  , squareUV
  , loadShaders
  , findULoc
  , bindVao
  , placeMatrix
  , makeFont
  , renderText
  
) where

import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import Graphics.Rendering.FreeType.Internal.Bitmap
import Graphics.Rendering.FreeType.Internal.Vector

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Graphics.Rendering.OpenGL.Raw.Core31

import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as M

import Foreign.C.String
import Foreign

import System.IO
import Data.Maybe
import Data.Foldable ( foldl' )
import Control.Monad ( liftM )

-- big square
square :: IO BufferObject
square = initGeometry . S.fromList $ map (*256) $
  concat [[-1, -1, 0],[ 1, -1, 0],[ 1,  1, 0]
         ,[ 1,  1, 0],[-1,  1, 0],[-1, -1, 0]]

-- texture coordinates
squareUV :: IO BufferObject
squareUV = initGeometry . S.fromList $
  concat [[ 0, 1],[ 1, 1],[ 1, 0]
         ,[ 1, 0],[ 0, 0],[ 0, 1]]

initGeometry :: S.Vector GLfloat -> IO BufferObject
initGeometry tris = do
  [vbo] <- genObjectNames 1
  bindBuffer ArrayBuffer $= Just vbo
  let len = fromIntegral $ S.length tris * sizeOf (S.head tris)
  S.unsafeWith tris $ \ptr ->
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

placeMatrix :: GLmatrix GLfloat -> GLint -> IO ()
placeMatrix mat uid = withMatrix mat $ \_ ptr -> glUniformMatrix4fv uid 1 0 ptr

type Font = FT_Face

makeFont :: String -> IO Font
makeFont filename = do
  libraryptr <- malloc
  library <- do
    errCode <- ft_Init_FreeType libraryptr
    print errCode
    peek libraryptr

  faceptr <- malloc
  withCString filename $ \str -> do
    errCode <- ft_New_Face library str 0 faceptr
    print errCode
    peek faceptr

renderText :: Font -> Int -> Int -> Int -> String -> IO ((Maybe TextureObject), Int)
renderText face dim lineHeight size string = do

  ft_Set_Pixel_Sizes face 0 $ fromIntegral size
  pen <- mallocForeignPtr
  withForeignPtr pen $ \p -> poke p FT_Vector
        { x = 128
        , y = 0}

  nullGlyph <- ft_Get_Char_Index face 0

  withForeignPtr pen $ \pp -> do
    pixels <- renderText' pp string nullGlyph
    let image = S.replicate (dim * dim) (0 :: Word8)
        buf = S.create $ do
            vec <- S.thaw image
            mapM_ (uncurry (M.write vec)) pixels
            return vec
    exts <- get glExtensions
    texture <- if "GL_EXT_texture_object" `elem` exts
                  then liftM listToMaybe $ genObjectNames 1
                  else return Nothing
    textureBinding Texture2D $= texture

    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
    textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
    S.unsafeWith buf $ texImage2D Nothing NoProxy 0 RGBA'
                                  (TextureSize2D (fromIntegral dim) (fromIntegral dim))
                                  0 . PixelData Alpha UnsignedByte
    return (texture, foldl' max 0 (map ((flip mod dim).fst) pixels))
  where
    renderText' _ [] _ = return []
    renderText' pen (c:xc) prev = do
      slot <- peek $ glyph face

      char <- ft_Get_Char_Index face (fromIntegral . fromEnum $ c)

      kerningptr <- malloc
      kerning <- do
        ft_Get_Kerning face prev char (fromIntegral ft_KERNING_DEFAULT) kerningptr
        peek kerningptr

      pen'' <- peek pen
      poke pen FT_Vector { x = x kerning + x pen''
                           , y = y kerning + y pen'' }

      ft_Set_Transform face nullPtr pen

      ft_Load_Glyph face char $ ft_LOAD_RENDER .|. fromIntegral ft_LOAD_TARGET_NORMAL

      v <- peek $ advance slot
      pen' <- peek pen
      poke pen FT_Vector { x = x v + x pen'
                           , y = y v + y pen' }

      (FT_Bitmap height width _ pixels _ _ _ _) <- peek $ GS.bitmap slot
      left <- liftM fromIntegral $ peek $ bitmap_left slot
      top <- liftM fromIntegral $ peek $ bitmap_top slot

      let xMax = left + fromIntegral width
          yMax = lineHeight - top + fromIntegral height
      return $ concatMap (\(i,p) -> map (\(j,q) ->
                  let index = q * width + p
                      imageIndex = fromIntegral $ j * dim + i
                      b = unsafePerformIO $ peek $ pixels `plusPtr` fromIntegral index
                  in if b>0 then (imageIndex, b) else (0,0))
                    (zip [ lineHeight - top .. yMax - 1] [0 .. ]))
                    (zip [ left .. xMax - 1] [0 .. ])
                    ++ unsafePerformIO (renderText' pen xc char)