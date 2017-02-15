module PetaVision.Data.Convolution where

import           Control.Arrow
import           Control.Monad      as M
import           Data.Array.ST      as AST
import           Data.Array.Unboxed as AU

-- All arrays use zero-based index
{-# INLINE crossCorrelation2D #-}

crossCorrelation2D
  :: Int
  -> UArray (Int, Int) Double
  -> UArray (Int, Int) Double
  -> UArray (Int, Int) Double
crossCorrelation2D stride act weight =
  runSTUArray $
  do arr' <- newArray ((0, 0), (newNx - 1, newNy - 1)) 0
     M.mapM_
       (\(i, j) ->
           let (startX, startY) = join (***) (* stride) (i, j)
               w = amap (* (act AU.! (i, j))) weight
           in M.mapM_
                (\(a, b) -> do
                   let idx = (startX + a, startY + b)
                   x <- readArray arr' idx
                   writeArray arr' idx (x + (w AU.! (a, b)))) .
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
  -> UArray (Int, Int) Double
  -> UArray (Int, Int, Int) Double
  -> UArray (Int, Int, Int) Double
crossCorrelation25D stride act weight =
  runSTUArray $
  do arr' <- newArray ((0, 0, weightLbZ), (newNx - 1, newNy - 1, weightUbZ)) 0
     M.mapM_
       (\(i, j) ->
           let (startX, startY) = join (***) (* stride) (i, j)
               w = amap (* (act AU.! (i, j))) weight
           in M.mapM_
                (\(a, b, c) -> do
                   let idx = (startX + a, startY + b, c)
                   x <- readArray arr' idx
                   writeArray arr' idx (x + (w AU.! (a, b, c)))) .
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
