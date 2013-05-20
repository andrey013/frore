{-# LANGUAGE QuasiQuotes #-}

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

-- import qualified Data.Vector.Storable as S
-- import qualified Data.Vector.Storable.Mutable as M

import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

import Foreign.C.String
import Foreign

import System.IO
import Data.Maybe
import Data.Foldable ( foldl' )
import Control.Monad ( liftM, (>=>) )

-- big square
square :: IO BufferObject
square = initGeometry . fromList (Z :. (18::Int)) $ Prelude.map (*512) $
  concat [[-1, -1, 0],[ 1, -1, 0],[ 1,  1, 0]
         ,[ 1,  1, 0],[-1,  1, 0],[-1, -1, 0]]

-- texture coordinates
squareUV :: IO BufferObject
squareUV = initGeometry . fromList (Z :. (12::Int)) $
  concat [[ 0, 1],[ 1, 1],[ 1, 0]
         ,[ 1, 0],[ 0, 0],[ 0, 1]]

initGeometry :: Array F DIM1 GLfloat -> IO BufferObject
initGeometry tris = do
  [vbo] <- genObjectNames 1
  bindBuffer ArrayBuffer $= Just vbo
  let len = fromIntegral $ (fromIntegral . Data.Array.Repa.size . extent) tris * sizeOf (tris ! (Z :. 0))
  let fptr = toForeignPtr tris
  withForeignPtr fptr $ \ptr ->
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

emptySquareArray :: Int -> Array F DIM2 Word8
emptySquareArray d = fromList (Z :. d :. d) (take (d * d) . cycle $ [0])

renderText :: Font -> Int -> Int -> Int -> String -> IO (Maybe TextureObject)
renderText face dim lineHeight size string = do

  ft_Set_Pixel_Sizes face 0 $ fromIntegral size
  pen <- mallocForeignPtr
  withForeignPtr pen $ \p -> poke p FT_Vector
        { x = 128
        , y = 0}

  nullGlyph <- ft_Get_Char_Index face 0

  withForeignPtr pen $ \pp -> do
    glyphs <- renderText' pp string nullGlyph
    let background = emptySquareArray dim
    let glyph = image . head $ glyphs
    canvas <- (align background >=> promote) glyph
    r <- (blurV  >=> demote) canvas
    g <- (blurH  >=> demote) canvas
    b <- (blurD1 >=> demote) canvas
    a <- (blurD2 >=> demote) canvas
    buf <- computeP $ interleave4 r g b a
    let ptr = toForeignPtr buf
    exts <- get glExtensions
    texture <- if "GL_EXT_texture_object" `elem` exts
                  then liftM listToMaybe $ genObjectNames 1
                  else return Nothing
    textureBinding Texture2D $= texture

    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
    textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
    withForeignPtr ptr $ texImage2D Nothing NoProxy 0 RGBA'
                                  (TextureSize2D (fromIntegral dim) (fromIntegral dim))
                                  0 . PixelData RGBA UnsignedByte
    return texture
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

      ft_Load_Glyph face char $ ft_LOAD_RENDER .|. ft_LOAD_NO_HINTING .|. fromIntegral ft_LOAD_TARGET_LIGHT

      v <- peek $ advance slot
      pen' <- peek pen
      poke pen FT_Vector { x = x v + x pen'
                           , y = y v + y pen' }

      (FT_Bitmap h w _ pixels _ _ _ _) <- peek $ GS.bitmap slot
      left <- liftM fromIntegral $ peek $ bitmap_left slot
      top <- liftM fromIntegral $ peek $ bitmap_top slot

      let width = fromIntegral w
          height = fromIntegral h
      bitmap <- ptr2repa (castPtr pixels) height width
      return $ Glyph bitmap : unsafePerformIO (renderText' pen xc char)

ptr2repa :: Ptr Word8 -> Int -> Int -> IO (Array F DIM2 Word8)
ptr2repa p i j = do
    fp <- newForeignPtr_ p
    return $ fromForeignPtr (Z :. i :. j) fp

data Glyph = Glyph
  { image  :: Array F DIM2 Word8
  -- , advance :: Int
  }

align :: Monad m => Array F DIM2 Word8 -> Array F DIM2 Word8 -> m (Array F DIM2 Word8)
align back arr = computeP $ backpermuteDft back
      (\(Z :. i :. j) -> if (i-10>=0)&&(j-10>=0)&&(i-10<height)&&(j-10<width)
                         then Just $ Z :. i-10 :. j-10
                         else Nothing
      ) arr
 where (Z :. height :. width) = extent arr

blurH :: Monad m => Array F DIM2 Double -> m (Array F DIM2 Double)
blurH arr
 = computeP $ smap (/ 18)
            $ forStencil2 (BoundConst 0) arr
              [stencil2|   0  0  0  0  0
                           0  0  0  0  0
                           1  2 12  2  1
                           0  0  0  0  0
                           0  0  0  0  0 |]

blurV :: Monad m => Array F DIM2 Double -> m (Array F DIM2 Double)
blurV arr
 = computeP $ smap (/ 18)
            $ forStencil2 (BoundConst 0) arr
              [stencil2|   0  0  1  0  0
                           0  0  2  0  0
                           0  0 12  0  0
                           0  0  2  0  0
                           0  0  1  0  0 |]

blurD1 :: Monad m => Array F DIM2 Double -> m (Array F DIM2 Double)
blurD1 arr
 = computeP $ smap (/ 12)
            $ forStencil2 (BoundConst 0) arr
              [stencil2|   1  0  0  0  0
                           0  2  0  0  0
                           0  0  6  0  0
                           0  0  0  2  0
                           0  0  0  0  1 |]

blurD2 :: Monad m => Array F DIM2 Double -> m (Array F DIM2 Double)
blurD2 arr
 = computeP $ smap (/ 12)
            $ forStencil2 (BoundConst 0) arr
              [stencil2|   0  0  0  0  1
                           0  0  0  2  0
                           0  0  6  0  0
                           0  2  0  0  0
                           1  0  0  0  0 |]

promote :: Monad m => Array F DIM2 Word8 -> m (Array F DIM2 Double)
promote arr = computeP $ Data.Array.Repa.map ffs arr
 where
  ffs :: Word8 -> Double
  ffs x =  fromIntegral (fromIntegral x :: Int)

demote  :: Monad m => Array F DIM2 Double -> m (Array F DIM2 Word8)
demote arr = computeP $ Data.Array.Repa.map ffs arr
 where
  ffs   :: Double -> Word8
  ffs x =  fromIntegral (truncate x :: Int)
