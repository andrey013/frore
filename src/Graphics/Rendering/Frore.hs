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

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

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
import Control.Monad ( liftM, liftM2, (>=>), (<=<), when )
import Control.Monad.ST

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

emptySquareArray :: Int -> Array U DIM2 Word8
emptySquareArray d = fromList (Z :. d :. d) (take (d * d) . cycle $ [0])

emptyArray :: DIM2 -> Array U DIM2 Word8
emptyArray d@(Z :. h :. w) = fromList (Z :. h + 4 :. w + 4) (take ((h + 4) * (w + 4)) . cycle $ [0])

drawTexture buf mode = do
  let ptr = toForeignPtr buf
  texture <- liftM listToMaybe $ genObjectNames 1
  textureBinding Texture2D $= texture
  textureFilter Texture2D $= ((mode, Nothing), mode)
  textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
  withForeignPtr ptr $ texImage2D Nothing NoProxy 0 RGBA'
                                  (TextureSize2D (fromIntegral 64) (fromIntegral 64))
                                  0 . PixelData RGBA UnsignedByte
  return texture

renderText :: Font -> Int -> Int -> Int -> String -> IO (Maybe TextureObject, Maybe TextureObject)
renderText face dim lineHeight size string = do

  ft_Set_Pixel_Sizes face 0 $ fromIntegral size
  pen <- mallocForeignPtr
  withForeignPtr pen $ \p -> poke p FT_Vector
        { x = 128
        , y = 0}

  nullGlyph <- ft_Get_Char_Index face 0

  withForeignPtr pen $ \pp -> do
    glyphs <- renderText' pp string nullGlyph
    let glyph = image . head $ glyphs
        dims@(Z :. h :. w) = extent glyph
    let background = emptySquareArray $ 64
    canvas <- align background glyph
    let r = distCalcV canvas
        g = distCalcH canvas
        b = distCalcD1 canvas
        a = distCalcD2 canvas
    buf1 <- computeP $ interleave4 r g b a
    buf2 <- computeP $ interleave4 canvas canvas canvas canvas
    liftM2 (,) (drawTexture buf1 Linear') (drawTexture buf2 Nearest)
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
        -- $ ft_LOAD_RENDER .|. fromIntegral ft_LOAD_TARGET_NORMAL
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
      liftM (Glyph bitmap :) $ renderText' pen xc char

ptr2repa :: Ptr Word8 -> Int -> Int -> IO (Array F DIM2 Word8)
ptr2repa p i j = do
    fp <- newForeignPtr_ p
    return $ fromForeignPtr (Z :. i :. j) fp

data Glyph = Glyph
  { image  :: Array F DIM2 Word8
  -- , advance :: Int
  }

align :: Monad m => Array U DIM2 Word8 -> Array F DIM2 Word8 -> m (Array U DIM2 Word8)
align back arr = computeP $ backpermuteDft back
      (\(Z :. i :. j) -> if (i>=marginTop)&&(j>=marginLeft) &&(i<h1+marginTop)&&(j<w1+marginLeft)
                         then Just $ Z :. i-marginTop :. j-marginLeft
                         else Nothing
      ) arr
 where (Z :. h1 :. w1) = extent arr
       (Z :. h2 :. w2) = extent back
       marginLeft = (w2 - w1) `div` 2
       marginTop = (h2 - h1) `div` 2

distCalcH :: Array U DIM2 Word8 -> Array U DIM2 Word8
distCalcH arr = fromUnboxed e . calcdf index (+ 1) (subtract 1) . toUnboxed $ arr
  where e@(Z :. h :. w) = extent arr
        index v i = fromMaybe 0 $ v U.!? i

distCalcV :: Array U DIM2 Word8 -> Array U DIM2 Word8
distCalcV arr = fromUnboxed e . calcdf index (+ w) (subtract w) . toUnboxed $ arr
  where e@(Z :. h :. w) = extent arr
        index v i = fromMaybe 0 $ v U.!? i

distCalcD1 :: Array U DIM2 Word8 -> Array U DIM2 Word8
distCalcD1 arr = fromUnboxed e . calcdf index (+ (w+1)) (subtract (w+1)) . toUnboxed $ arr
  where e@(Z :. h :. w) = extent arr
        index :: U.Vector Word8 -> Int -> Word8
        index v i = let t = fromIntegral $ fromMaybe 0 $ v U.!? (i - w)
                        b = fromIntegral $ fromMaybe 0 $ v U.!? (i + w)
                        l = fromIntegral $ fromMaybe 0 $ v U.!? (i - 1)
                        r = fromIntegral $ fromMaybe 0 $ v U.!? (i + 1)
                        c = fromIntegral $ fromMaybe 0 $ v U.!? i
                    in round $ 0.042893219 * (t + b + l + r) + 0.828427125 * c

distCalcD2 :: Array U DIM2 Word8 -> Array U DIM2 Word8
distCalcD2 arr = fromUnboxed e . calcdf index (+ (w-1)) (subtract (w-1)) . toUnboxed $ arr
  where e@(Z :. h :. w) = extent arr
        index :: U.Vector Word8 -> Int -> Word8
        index v i = let t = fromIntegral $ fromMaybe 0 $ v U.!? (i - w)
                        b = fromIntegral $ fromMaybe 0 $ v U.!? (i + w)
                        l = fromIntegral $ fromMaybe 0 $ v U.!? (i - 1)
                        r = fromIntegral $ fromMaybe 0 $ v U.!? (i + 1)
                        c = fromIntegral $ fromMaybe 0 $ v U.!? i
                    in round $ 0.042893219 * (t + b + l + r) + 0.828427125 * c

calcdf :: (U.Vector Word8 -> Int -> Word8) -> (Int -> Int) -> (Int -> Int) -> U.Vector Word8 -> U.Vector Word8
calcdf indexFunction n p orig =
  U.modify (\v -> do
    mapM_ (dist p v) [0 .. U.length orig - 1]
    mapM_ (dist n v) [U.length orig - 1, U.length orig - 2 .. 0]
  ) $ U.replicate (U.length orig) 0
  where
    dist f v i  = do
      let prev = indexFunction orig $ f i -- fromMaybe 0 $ o U.!? (f i)
          curr = indexFunction orig i -- o U.! i
          l = U.length orig
      prevValue <- if f i >= l || f i < 0 then return 0 else UM.read v (f i)
      currValue <- UM.read v i
      let currValueInside = if currValue == 0 then 255 else currValue
      UM.write v i $
        if (curr <= boundary)
        then if prev > boundary
             then max currValue $ 127 - ((255-prev) `div` (divider*2))
             else if prevValue - dd > 200
                  then max currValue $ 0
                  else max currValue $ prevValue - dd
        else if prev <= boundary
             then min currValueInside $ 127 + ((curr) `div` (divider*2))
             else if prevValue + dd < 50
                  then min currValueInside $ 255
                  else min currValueInside $ prevValue + dd
    dd = 16
    divider = 8
    boundary = 0
