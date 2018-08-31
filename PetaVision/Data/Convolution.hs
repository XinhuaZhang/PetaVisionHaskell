{-# LANGUAGE FlexibleContexts #-}
module PetaVision.Data.Convolution where

import           Control.Arrow
import           Control.Monad      as M
import           Data.Array.IArray  (amap)
import           Data.Array.Repa    as R
import           Data.Array.ST      as AST
import           Data.Array         as Arr

-- All arrays use zero-based index
{-# INLINE crossCorrelation2D #-}

crossCorrelation2D
  :: Int
  -> Arr.Array (Int, Int) Double
  -> Arr.Array (Int, Int) Double
  -> Arr.Array (Int, Int) Double
crossCorrelation2D stride act weight =
  runSTArray $
  do arr' <- newArray ((0, 0), (newNx - 1, newNy - 1)) 0
     M.mapM_
       (\(i, j) ->
           let (startX, startY) = join (***) (* stride) (i, j)
               w = amap (* (act Arr.! (i, j))) weight
           in M.mapM_
                (\(a, b) -> do
                   let idx = (startX + a, startY + b)
                   x <- readArray arr' idx
                   writeArray arr' idx (x + (w Arr.! (a, b)))) .
              range . bounds $
              w) .
       range . bounds $
       act
     return arr'
  where
    ((actLbX, actLbY), (actUbX, actUbY)) = bounds act
    ((weightLbX, weightLbY), (weightUbX, weightUbY)) = bounds weight
    len a b = b - a + 1
    actNx = len actLbX actUbX
    actNy = len actLbY actUbY
    weightNx = len weightLbX weightUbX
    weightNy = len weightLbY weightUbY
    newNx = (actNx - 1) * stride + weightNx
    newNy = (actNy - 1) * stride + weightNy

-- Array layout is ny x nx x nf
{-# INLINE crossCorrelation25D #-}

crossCorrelation25D
  :: Int
  -> Arr.Array (Int, Int) Double
  -> Arr.Array (Int, Int, Int) Double
  -> Arr.Array (Int, Int, Int) Double
crossCorrelation25D stride act weight =
  runSTArray $
  do arr' <- newArray ((0, 0, weightLbZ), (newNx - 1, newNy - 1, weightUbZ)) 0
     M.mapM_
       (\(i, j) ->
           let (startX, startY) = join (***) (* stride) (i, j)
               w = amap (* (act Arr.! (i, j))) weight
           in M.mapM_
                (\(a, b, c) -> do
                   let idx = (startX + a, startY + b, c)
                   x <- readArray arr' idx
                   writeArray arr' idx (x + (w Arr.! (a, b, c)))) .
              range . bounds $
              w) .
       range . bounds $
       act
     return arr'
  where
    ((actLbX, actLbY), (actUbX, actUbY)) = bounds act
    ((weightLbX, weightLbY, weightLbZ), (weightUbX, weightUbY, weightUbZ)) =
      bounds weight
    len a b = b - a + 1
    actNx = len actLbX actUbX
    actNy = len actLbY actUbY
    weightNx = len weightLbX weightUbX
    weightNy = len weightLbY weightUbY
    newNx = (actNx - 1) * stride + weightNx
    newNy = (actNy - 1) * stride + weightNy


{-# INLINE array2RepaArray3 #-}

array2RepaArray3 :: Arr.Array (Int, Int, Int) Double -> R.Array U DIM3 Double
array2RepaArray3 arr =
  let ((lb1, lb2, lb3), (ub1, ub2, ub3)) = bounds arr
  in fromListUnboxed
       (Z :. (ub1 - lb1 + 1) :. (ub2 - lb2 + 1) :. (ub3 - lb3 + 1)) .
     elems $
     arr

{-# INLINE repaArray2Array2 #-}  

repaArray2Array2
  :: (R.Source s Double)
  => R.Array s DIM2 Double -> Arr.Array (Int, Int) Double
repaArray2Array2 arr =
  let (Z :. ny' :. nx') = extent arr
  in listArray ((0, 0), (ny' - 1, nx' - 1)) . R.toList $ arr
