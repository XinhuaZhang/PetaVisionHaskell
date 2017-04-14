{-# LANGUAGE QuasiQuotes #-}
module PetaVision.Utility.Array where

import           Data.Array.Repa              as R
import           Data.Array.Repa.Stencil      as R
import           Data.Array.Repa.Stencil.Dim2 as R
import           Data.List                    as L
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU

-- array layerout: ny x nx x nf
{-# INLINE extractFeaturePoint #-}

extractFeaturePoint
  :: (Source s e, Unbox e)
  => Array s DIM3 e -> [VU.Vector e]
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
  => Array s DIM3 e -> [VU.Vector e]
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


{-# INLINE bicubicInterpolation #-}

bicubicInterpolation
  :: [R.Array U DIM2 Double] -> (Double,Double) ->  (Double,Double) -> Double
bicubicInterpolation ds (minVal, maxVal) (y, x)
  | (x < 0) ||
      (x > (fromIntegral nx - 1)) || (y < 0) || (y > (fromIntegral ny - 1)) = 0
  | result < minVal = minVal
  | result > maxVal = maxVal
  | otherwise = result
  where
    (Z :. ny :. nx) = extent . L.head $ ds
    x' = x - fromIntegral (floor x :: Int)
    y' = y - fromIntegral (floor y :: Int)
    idx =
      VU.fromListN
        4
        [ (floor y, floor x)
        , (floor y, ceiling x)
        , (ceiling y, floor x)
        , (ceiling y, ceiling x)
        ] :: VU.Vector (Int, Int)
    xs =
      VU.concat .
      L.map (\arr' -> VU.map (\(i, j) -> arr' R.! (Z :. i :. j)) idx) $
      ds
    alpha = V.map (VU.sum . VU.zipWith (*) xs) matrixA
    arr = fromListUnboxed (Z :. 4 :. 4) . V.toList $ alpha :: R.Array U DIM2 Double
    arr1 =
      R.traverse arr id (\f idx'@(Z :. j :. i) -> f idx' * (x' ^ i) * (y' ^ j))
    result = sumAllS arr1
    matrixA =
      V.fromListN 16 . L.map (VU.fromListN 16) $
      [ [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      , [-3, 3, 0, 0, -2, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      , [2, -2, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0]
      , [0, 0, 0, 0, 0, 0, 0, 0, -3, 3, 0, 0, -2, -1, 0, 0]
      , [0, 0, 0, 0, 0, 0, 0, 0, 2, -2, 0, 0, 1, 1, 0, 0]
      , [-3, 0, 3, 0, 0, 0, 0, 0, -2, 0, -1, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, -3, 0, 3, 0, 0, 0, 0, 0, -2, 0, -1, 0]
      , [9, -9, -9, 9, 6, 3, -6, -3, 6, -6, 3, -3, 4, 2, 2, 1]
      , [-6, 6, 6, -6, -3, -3, 3, 3, -4, 4, -2, 2, -2, -2, -1, -1]
      , [2, 0, -2, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, 2, 0, -2, 0, 0, 0, 0, 0, 1, 0, 1, 0]
      , [-6, 6, 6, -6, -4, -2, 4, 2, -3, 3, -3, 3, -2, -1, -2, -1]
      , [4, -4, -4, 4, 2, 2, -2, -2, 2, -2, 2, -2, 1, 1, 1, 1]
      ]

{-# INLINE computeDerivativeS #-}

computeDerivativeS
  :: R.Array U DIM2 Double -> [R.Array U DIM2 Double]
computeDerivativeS arr = arr : ds'
  where xStencil =
          [stencil2| 0 0 0
                     -1 0 1
                     0 0 0 |]
        yStencil =
          [stencil2| 0 -1 0
                     0 0 0
                     0 1 0 |]
        xyStencil =
          [stencil2| 1 0 -1
                     0 0 0
                     -1 0 1 |]
        ds =
          L.map (\s -> R.map (/ 2) $ mapStencil2 BoundClamp s arr)
                [xStencil,yStencil,xyStencil]
        ds' = L.map computeS ds
