module PetaVision.Data.Histogram1D where

import           Control.Monad                          as M
import           Data.Vector.Unboxed                    as VU
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy

data Histogram1D = Histogram1D
  { histogram1DStep  :: Double
  , histogram1DRange :: (Double,Double)
  , histogram1DHist  :: VU.Vector Double
  }

{-# INLINE computeRange #-}

computeRange :: VU.Vector Double -> (Double,Double)
computeRange vec = (VU.minimum vec, VU.maximum vec)

{-# INLINE histogram1d #-}

histogram1d :: Int -> (Double,Double) -> VU.Vector Double -> Histogram1D
histogram1d nbins (minVal, maxVal) vec =
  let step = (maxVal - minVal) / (fromIntegral nbins)
  in Histogram1D step (minVal, maxVal) .
     VU.accumulate (+) (VU.replicate nbins 0) .
     VU.map
       (\x ->
          if x == maxVal
            then (nbins - 1, 1)
            else (floor (x / step), 1)) $
     vec


{-# INLINE histogram1d' #-}

histogram1d' :: Histogram1D -> VU.Vector Double -> Histogram1D
histogram1d' (Histogram1D step (minVal, maxVal) hist) vec =
  let nbins = VU.length hist
      step = (maxVal - minVal) / (fromIntegral nbins)
  in Histogram1D step (minVal, maxVal) .
     VU.accumulate (+) hist .
     VU.map
       (\x ->
          if x == maxVal
            then (nbins - 1, 1)
            else (floor (x / step), 1)) $
     vec

{-# INLINE normalizeHistogram1D #-}

normalizeHistogram1D :: Histogram1D -> Histogram1D
normalizeHistogram1D (Histogram1D step (minV, maxV) vec) =
  Histogram1D step (minV, maxV) . VU.map (/ VU.sum vec) $ vec

plotHistogram1D :: FilePath -> String -> Histogram1D -> IO ()
plotHistogram1D filePath name (Histogram1D step (minV, maxV) vec) = do
  toFile def filePath $ do
    layout_title .= name
    plot
      (line
         ""
         [ VU.toList . VU.imap (\i x -> (minV + (fromIntegral i * step), x)) $
           vec
         ])


plotHistogram1DList :: FilePath -> Double -> [String] -> [Histogram1D] -> IO ()
plotHistogram1DList filePath threshold names xs = do
  toFile def filePath $
    -- layout_title .= name
   do
    M.zipWithM_
      (\(Histogram1D step (minV, maxV) vec) name ->
         plot
           (line
              name
              [ VU.toList .
                VU.imap (\i x -> (minV + (fromIntegral i * step), x)) .
                VU.take (floor $ threshold / step) $
                vec
              ]))
      xs
      names
