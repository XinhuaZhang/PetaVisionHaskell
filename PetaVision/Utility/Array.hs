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

{-# INLINE crop #-}

crop
  :: (Source s e, Shape sh)
  => [Int] -> [Int] -> Array s sh e -> Array D sh e
crop start len arr
  | L.any (< 0) start ||
      L.or (L.zipWith3 (\x y z -> x > (z - y)) start len dList) =
    error $
    "Crop out of boundary!\n" L.++ show start L.++ "\n" L.++ show len L.++ "\n" L.++
    show dList
  | otherwise =
    R.backpermute
      (shapeOfList len)
      (shapeOfList . L.zipWith (+) start . listOfShape)
      arr
  where
    dList = listOfShape $ extent arr

{-# INLINE cropUnsafe #-}

cropUnsafe
  :: (Source s e, Shape sh)
  => [Int] -> [Int] -> Array s sh e -> Array D sh e
cropUnsafe start len =
  R.backpermute
    (shapeOfList len)
    (shapeOfList . L.zipWith (+) start . listOfShape)

-- factor = 2^n, n = 0,1,..
-- the first factor in the list corresponds to the inner-most (right-most) dimension.
{-# INLINE downsample #-}

downsample
  :: (Source s e, Shape sh)
  => [Int] -> Array s sh e -> Array D sh e
downsample factorList arr
  | L.all (== 1) factorList = delay arr
  | L.any (< 1) newDList =
    error $
    "Downsample factors are too large." L.++ show dList L.++ "\n" L.++ show factorList
  | otherwise =
    R.backpermute
      (shapeOfList newDList)
      (shapeOfList . L.zipWith (*) factorList . listOfShape)
      arr
  where
    dList = listOfShape . extent $ arr
    newDList = L.zipWith div dList factorList

{-# INLINE downsampleUnsafe #-}

downsampleUnsafe
  :: (Source s e, Shape sh)
  => [Int] -> Array s sh e -> Array D sh e
downsampleUnsafe factorList arr =
  R.backpermute newSh (shapeOfList . L.zipWith (*) factorList . listOfShape) arr
  where
    dList = listOfShape $ extent arr
    newSh = shapeOfList $ L.zipWith div dList factorList
