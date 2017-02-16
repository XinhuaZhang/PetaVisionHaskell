{-# LANGUAGE FlexibleContexts #-}
module PetaVision.Data.Image.Utility where

import           Data.Array.Repa          as R
import           Data.List                as L
import           PetaVision.PVPFile.Types
import           PetaVision.Utility.Array


-- Image layout: ny x nx x nf

-- convert an image into a square image by cropping the long size
{-# INLINE squareImage #-}

squareImage :: (Source s e) => Array s DIM3 e -> Array D DIM3 e
squareImage arr
  | ny' == nx' = delay arr
  | ny' > nx' = crop [0, 0, margin] [nf', nx', nx'] arr
  | otherwise = crop [0, margin, 0] [nf', ny', ny'] arr
  where
    (Z :. ny' :. nx' :. nf') = extent arr
    longSize = max nx' ny'
    shortSize = min nx' ny'
    margin = div (longSize - shortSize) 2

{-# INLINE rescaleImage2D #-}

rescaleImage2D
  :: (Source s Double)
  => (Int, Int) -> (Double, Double) -> Array s DIM2 Double -> Array D DIM2 Double
rescaleImage2D (newNy, newNx) bound arr =
  fromFunction
    (Z :. newNy :. newNx)
    (\(Z :. j :. i) ->
        bicubicInterpolation
          ds
          bound
          (fromIntegral j * ratioY, fromIntegral i * ratioX))
  where
    ds = computeDerivativeS . computeUnboxedS . delay $ arr
    (Z :. ny' :. nx') = extent arr
    ratioX = fromIntegral (nx' - 1) / fromIntegral (newNx - 1) 
    ratioY = fromIntegral (ny' - 1) / fromIntegral (newNy - 1) 

{-# INLINE rescaleImage25D #-}

rescaleImage25D
  :: (Source s Double)
  => (Int, Int) -> (Double, Double) -> Array s DIM3 Double -> Array D DIM3 Double
rescaleImage25D newSize bound arr =
  L.foldl1' append .
  L.map
    (\i ->
        extend (Z :. All :. All :. (1 :: Int)) .
        rescaleImage2D newSize bound . R.slice arr $
        (Z :. All :. All :. i)) $
  [0 .. nf' - 1]
  where
    (Z :. _ :. _ :. nf') = extent arr

{-# INLINE image2PVPData #-}

image2PVPData
  :: (Source s Double)
  => Array s DIM3 Double -> PVPOutputData
image2PVPData arr =
  PVP_OUTPUT_NONSPIKING_ACT (PVPDimension nx' ny' nf') .
  R.toUnboxed . computeS . delay $
  arr
  where
    (Z :. ny' :. nx' :. nf') = extent arr
