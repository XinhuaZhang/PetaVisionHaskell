module PetaVision.Utility.Array where

import           Data.Array.Repa     as R
import           Data.List           as L
import           Data.Vector.Unboxed as VU

-- array layerout: ny x nx x nf

{-# INLINE extractFeaturePoint #-}

extractFeaturePoint
  :: (Source s e, Unbox e)
  => Array s DIM3 e -> [Vector e]
extractFeaturePoint arr =
  let (Z :. ny' :. nx' :. _) = extent arr
  in L.map
       (toUnboxed . computeS . R.slice arr)
       [ Z :. j :. i :. All
       | i <- [0 .. nx' - 1]
       , j <- [0 .. ny' - 1] ]
       
{-# INLINE extractFeatureMap #-}

extractFeatureMap
  :: (Source s e, Unbox e)
  => Array s DIM3 e -> [Vector e]
extractFeatureMap arr =
  let (Z :. _ :. _ :. nf') = extent arr
  in L.map
       (toUnboxed . computeS . R.slice arr)
       [ Z :. All :. All :. i
       | i <- [0 .. nf' - 1] ]
