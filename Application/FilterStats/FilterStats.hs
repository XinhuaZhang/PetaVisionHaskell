{-# LANGUAGE BangPatterns #-}
module Application.FilterStats.FilterStats where

import           Control.Arrow
import           Data.List                              as L
import           Data.Maybe
import           Data.Vector.Unboxed                    as VU
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy


{-# INLINE valueRange #-}
valueRange :: (Int, VU.Vector Double) -> (Double, Double)
valueRange (_, vec) = VU.minimum *** VU.maximum $ (vec, vec)


{-# INLINE meanVar #-}
meanVar :: (Int, VU.Vector Double) -> (Double,Double)
meanVar (_, vec) = (mu, sigma)
  where
    !len = VU.length vec
    !mu = VU.sum vec / fromIntegral len
    !sigma =
      (VU.sum . VU.map (\x -> (x - mu) ^ (2 :: Int)) $ vec) / fromIntegral len

plotHist
  :: Bool
  -> (Int, VU.Vector Double)
  -> (Double, Double)
  -> Int
  -> String
  -> FilePath
  -> IO ()
plotHist plotZero (nz, vec) (a, b) nbins title filePath =
  toFile def filePath $
  do layout_title .= title
     plot (line "Histogram" [VU.toList . VU.zip indexVec $ normalizedHist])
  where
    !width = (b - a) / fromIntegral nbins
    !indexVec = VU.generate nbins (\i -> a + fromIntegral i * width)
    !eleIndexVec =
      VU.filter (\x -> x >= 0 && x <= nbins - 1) .
      VU.map (\x -> floor $ (x - a) / width :: Int) $
      vec
    !hist =
      VU.accumulate (+) (VU.replicate nbins 0) . VU.zip eleIndexVec $
      VU.replicate (VU.length eleIndexVec) (1 :: Int)
    !hist1 = if plotZero
                then VU.accum (+) hist [(0,nz)]
                else hist
    !s = VU.sum hist1
    !normalizedHist =
      VU.map (\x -> fromIntegral x / fromIntegral s) hist1 :: VU.Vector Double
