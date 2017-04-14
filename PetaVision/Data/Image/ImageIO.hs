{-# LANGUAGE BangPatterns #-}
module PetaVision.Data.Image.ImageIO where

import           Codec.Picture
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Conduit                 as C
import           Data.Conduit.List            as CL
import           Data.Word
import           GHC.Float

readImagePathList :: FilePath -> IO [String]
readImagePathList = fmap lines . readFile

imagePathSource :: FilePath -> C.Source (ResourceT IO) FilePath
imagePathSource filePath = do
  pathList <- liftIO $ readImagePathList filePath
  sourceList pathList

readImageConduit :: Bool -> Conduit FilePath (ResourceT IO) (Array D DIM3 Double)
readImageConduit isColor =
  awaitForever
    (\filePath ->
       do buffer <- liftIO $ readImage filePath
          case buffer of
            Left msg -> error msg
            Right dImg ->
              let arr =
                    if isColor
                       then case dImg of
                              ImageY8 img ->
                                fromFunction
                                  (Z :. imageHeight img :. imageWidth img :.
                                   (1 :: Int))
                                  (\(Z :. j :. i :. _) ->
                                     fromIntegral $ pixelAt img i j :: Double)
                              ImageY16 img ->
                                fromFunction
                                  (Z :. imageHeight img :. imageWidth img :.
                                   (1 :: Int))
                                  (\(Z :. j :. i :. _) ->
                                     fromIntegral $ pixelAt img i j :: Double)
                              ImageYF img ->
                                fromFunction
                                  (Z :. imageHeight img :. imageWidth img :.
                                   (1 :: Int))
                                  (\(Z :. j :. i :. _) ->
                                     float2Double $ pixelAt img i j :: Double)
                              ImageRGB8 img ->
                                fromFunction
                                  (Z :. imageHeight img :. imageWidth img :.
                                   (3 :: Int))
                                  (\(Z :. j :. i :. k) ->
                                     let !(PixelRGB8 r g b) = pixelAt img i j
                                     in case k of
                                          0 -> fromIntegral r
                                          1 -> fromIntegral g
                                          2 -> fromIntegral b
                                          _ ->
                                            error "readImageConduit: dimension error.")
                              ImageRGB16 img ->
                                fromFunction
                                  (Z :. imageHeight img :. imageWidth img :.
                                   (3 :: Int))
                                  (\(Z :. j :. i :. k) ->
                                     let !(PixelRGB16 r g b) = pixelAt img i j
                                     in case k of
                                          0 -> fromIntegral r
                                          1 -> fromIntegral g
                                          2 -> fromIntegral b
                                          _ ->
                                            error "readImageConduit: dimension error.")
                              ImageRGBF img ->
                                fromFunction
                                  (Z :. imageHeight img :. imageWidth img :.
                                   (3 :: Int))
                                  (\(Z :. j :. i :. k) ->
                                     let !(PixelRGBF r g b) = pixelAt img i j
                                     in case k of
                                          0 -> float2Double r
                                          1 -> float2Double g
                                          2 -> float2Double b
                                          _ ->
                                            error "readImageConduit: dimension error.")
                              img ->
                                let !rgbImg = convertRGB8 img
                                in fromFunction
                                     (Z :. imageHeight rgbImg :.
                                      imageWidth rgbImg :.
                                      (3 :: Int))
                                     (\(Z :. j :. i :. k) ->
                                        let !(PixelRGB8 r g b) =
                                              pixelAt rgbImg i j
                                        in case k of
                                             0 -> fromIntegral r
                                             1 -> fromIntegral g
                                             2 -> fromIntegral b
                                             _ ->
                                               error "readImageConduit: dimension error.")
                       else case dImg of
                              ImageY8 img ->
                                fromFunction
                                  (Z :. imageHeight img :. imageWidth img :.
                                   (1 :: Int))
                                  (\(Z :. j :. i :. _) ->
                                     fromIntegral $ pixelAt img i j :: Double)
                              ImageY16 img ->
                                fromFunction
                                  (Z :. imageHeight img :. imageWidth img :.
                                   (1 :: Int))
                                  (\(Z :. j :. i :. _) ->
                                     fromIntegral $ pixelAt img i j :: Double)
                              ImageYF img ->
                                fromFunction
                                  (Z :. imageHeight img :. imageWidth img :.
                                   (1 :: Int))
                                  (\(Z :. j :. i :. _) ->
                                     float2Double $ pixelAt img i j :: Double)
                              ImageRGB8 img ->
                                fromFunction
                                  (Z :. imageHeight img :. imageWidth img :.
                                   (1 :: Int))
                                  (\(Z :. j :. i :. _) ->
                                     let !(PixelRGB8 r g b) = pixelAt img i j
                                     in rgb2Gray (fromIntegral (maxBound :: Word8))
                                                 (fromIntegral r)
                                                 (fromIntegral g)
                                                 (fromIntegral b))
                              ImageRGB16 img ->
                                fromFunction
                                  (Z :. imageHeight img :. imageWidth img :.
                                   (1 :: Int))
                                  (\(Z :. j :. i :. _) ->
                                     let !(PixelRGB16 r g b) = pixelAt img i j
                                     in rgb2Gray (fromIntegral (maxBound :: Word16))
                                                 (fromIntegral r)
                                                 (fromIntegral g)
                                                 (fromIntegral b))
                              ImageRGBF img ->
                                fromFunction
                                  (Z :. imageHeight img :. imageWidth img :.
                                   (1 :: Int))
                                  (\(Z :. j :. i :. _) ->
                                     let !(PixelRGBF r g b) = pixelAt img i j
                                     in rgb2Gray 1
                                                 (float2Double r)
                                                 (float2Double g)
                                                 (float2Double b))
                              img ->
                                let !rgbImg = convertRGB8 img
                                in fromFunction
                                     (Z :. imageHeight rgbImg :.
                                      imageWidth rgbImg :.
                                      (1 :: Int))
                                     (\(Z :. j :. i :. _) ->
                                        let !(PixelRGB8 r g b) =
                                              pixelAt rgbImg i j
                                        in rgb2Gray (fromIntegral (maxBound :: Word8))
                                                    (fromIntegral r)
                                                    (fromIntegral g)
                                                    (fromIntegral b))
              in yield arr)

{-# INLINE rgb2Gray #-}
rgb2Gray :: Double -> Double -> Double -> Double -> Double
rgb2Gray bound r g b
  | yLinear <= 0.0031308 = 12.92 * yLinear
  | otherwise = 1.055 * (yLinear ** (1 / 2.4)) - 0.055
  where
    !yLinear =
      0.2126 * func bound r + 0.7152 * func bound g + 0.0722 * func bound b

{-# INLINE func #-}
func :: Double -> Double -> Double
func bound x
  | y < 0.04045 = y / 12.92
  | otherwise = ((y + 0.055) / 1.055) ** 2.4
  where !y = x / bound


