{-# LANGUAGE DeriveGeneric #-}
module Application.GMM.Representation
  (DataVec
  ,fromListDense
  ,fromListSparse
  ,toListDense
  ,toListSparse
  ,dense2Sparse
  ,sparse2Dense
  ,sumVec
  ,productVec
  ,findVec
  ,lengthVec
  ,powVec
  ,addFoldVec
  ,scalarMulVec)
  where

import           Control.DeepSeq     as DS
import           Data.Binary
import           Data.List           as L
import           Data.Maybe
import           Data.Vector.Unboxed as VU
import Data.Vector as V
import           GHC.Generics

data DataVec a
  = DenseVec !(VU.Vector a)
  | SparseVec !Int ![(Int,a)]
  deriving (Show,Generic)

instance (Binary a
         ,Unbox a) =>
         Binary (DataVec a) where
  put (DenseVec vec) =
    do putWord8 0
       put $ VU.toList vec
  put (SparseVec n xs) =
    do putWord8 1
       put n
       put xs
  get =
    do t <- getWord8
       case t of
         0 ->
           do xs <- get
              return $! DenseVec . VU.fromList $ xs
         1 ->
           do n <- get
              xs <- get
              return $! SparseVec n xs

instance (NFData a) =>
         NFData (DataVec a) where
  rnf (DenseVec vec)   = rnf vec
  rnf (SparseVec n xs) = n `seq` xs `seq` ()
  
instance (Num a
         ,Unbox a
         ,NFData a) =>
         Num (DataVec a) where
  (+) = merge (+)
  (-) s1@(SparseVec _ _) d2@(DenseVec _) = merge (+) s1 $ mapVec negate d2
  (-) x y = merge (-) x y
  (*) x@ (SparseVec _ _) y @ (SparseVec _ _) = merge (*) (sparse2Dense x) (sparse2Dense y)
  (*) x@ (SparseVec _ _) y @ (DenseVec _) = merge (*) (sparse2Dense x) y
  (*) x@ (DenseVec _) y @ (SparseVec _ _) = merge (*) x (sparse2Dense y)
  (*) x y = merge (*) x y
  negate = mapVec negate 
  abs = mapVec abs
  signum = mapVec signum
  fromInteger = error "fromInteger: doesn't support DataVec."

instance (Num a
         ,Unbox a
         ,NFData a
         ,Fractional a) =>
         Fractional (DataVec a) where
  (/) _ (SparseVec _ _) = error "(/): denominator cannot be a sparse vector."
  (/) x@(SparseVec _ _) y = x * (mapVec (\a -> 1 / a) y)
  (/) x y = merge (/) x y
  fromRational = error "fromRational: doesn't support DataVec."



fromListDense :: (Unbox a)
              => [a] -> DataVec a
fromListDense = DenseVec . VU.fromList

fromListSparse :: Int -> [(Int,a)] -> DataVec a
fromListSparse n = SparseVec n . L.sortOn fst


toListDense :: (Unbox a
               ,Num a)
            => DataVec a -> [a]
toListDense (DenseVec vec) = VU.toList vec
toListDense (SparseVec n xs) = VU.toList . VU.accum (+) (VU.replicate n 0) $ xs

toListSparse :: (Unbox a
                ,Num a)
             => DataVec a -> [(Int,a)]
toListSparse (DenseVec vec) =
  VU.toList $
  VU.zip (VU.generate (VU.length vec)
                      id)
         vec
toListSparse (SparseVec _ xs) = xs

dense2Sparse :: (Unbox a)
             => DataVec a -> DataVec a
dense2Sparse (DenseVec vec) =
  SparseVec (VU.length vec) . VU.toList $
  VU.zip (VU.generate (VU.length vec)
                      id)
         vec
dense2Sparse _ = error "Input must be a dense vector."

sparse2Dense :: (Unbox a
                ,Num a)
             => DataVec a -> DataVec a
sparse2Dense (SparseVec n xs) = DenseVec $! VU.accum (+) (VU.replicate n 0) xs
sparse2Dense _                = error "Input must be a sparse vector."

mergeSparse
  :: (Num a,NFData a)
  => (a -> a -> a) -> [(Int,a)] -> [(Int,a)] -> [(Int,a)]
mergeSparse _ xs [] = xs
mergeSparse _ [] ys = ys
mergeSparse f ((ix,x):xs) ((iy,y):ys)
  | ix == iy = DS.force $ (ix,f x y) : mergeSparse f xs ys
  | ix < iy =
    DS.force $
    (ix,f x 0) :
    mergeSparse f
                xs
                ((iy,y) : ys)
  | otherwise =
    DS.force $
    (iy,f 0 y) :
    mergeSparse f
                ((ix,x) : xs)
                ys
-- merge, mapVec and foldSparseVec assume that function f with input 0 produces 0.
merge
  :: (Unbox a
     ,Num a
     ,NFData a)
  => (a -> a -> a) -> DataVec a -> DataVec a -> DataVec a
merge f (DenseVec d1) (DenseVec d2) = DenseVec $! VU.zipWith f d1 d2
merge f (SparseVec n1 s1) (SparseVec n2 s2)
  | n1 == n2 = SparseVec n1 $! mergeSparse f s1 s2
  | otherwise = error "Sparse vectors have different lengthes."
merge f (DenseVec d1) (SparseVec _ s2) = DenseVec $! VU.accum f d1 s2
merge f (SparseVec n s1) (DenseVec d2) =
  merge f
        (DenseVec d2)
        (SparseVec n s1)
        
mapVec :: (Unbox a)
       => (a -> a) -> DataVec a -> DataVec a
mapVec f (DenseVec vec)   = DenseVec $! VU.map f vec
mapVec f (SparseVec n xs) = SparseVec n $! L.map (\(i,x) -> (i,f x)) xs

-- fold a list of sparse vectors into a dense vector
foldSparseVec
  :: (Num a, Unbox a)
  => (a -> a -> a) -> [DataVec a] -> DataVec a
foldSparseVec f xs@((SparseVec n _):_) =
  DenseVec $!
  VU.accum f (VU.replicate n 0) $
  L.concatMap (\(SparseVec _ zs) -> zs)
              xs
foldSparseVec f ((DenseVec _):_) =
  error "foldSparseVec: input must be a sparse vector."


sumVec :: (Num a
       ,Unbox a)
    => DataVec a -> a
sumVec (DenseVec vec)   = VU.sum vec
sumVec (SparseVec _ xs) = L.foldl1' (+) . L.map snd $ xs

productVec :: (Num a
        ,Unbox a)
     => DataVec a -> a
productVec (DenseVec vec)   = VU.product vec
productVec (SparseVec _ xs) = L.foldl1' (*) . L.map snd $ xs


findVec :: (Unbox a) => (a -> Bool) -> DataVec a -> Maybe a
findVec f (DenseVec vec) = VU.find f vec
findVec f (SparseVec _ xs) = error "findVec: doesn't support sparse vector"
  -- case L.find (f . snd) xs of
  --   Nothing -> Nothing
  --   Just x  -> Just $! snd x

lengthVec :: (Unbox a) => DataVec a -> Int
lengthVec (DenseVec vec)   = VU.length vec
lengthVec (SparseVec n xs) = n

powVec :: (Fractional a
          ,Ord a
          ,Unbox a
          ,Floating a)
       => a -> DataVec a -> DataVec a
powVec n vec@(SparseVec _ xs)
  | n < 0 = error "powVec: n < 0 for sparse vector."
  | otherwise = mapVec (** n) vec
powVec n vec = mapVec (** n) vec


addFoldVec :: (Num a
              ,Unbox a
              ,NFData a)
           => V.Vector (DataVec a) -> DataVec a
addFoldVec xs =
  case V.head xs of
    DenseVec _ -> V.foldl1' (+) xs
    SparseVec _ _ -> foldSparseVec (+) . V.toList $ xs

scalarMulVec :: (Num a
                ,Unbox a
                ,NFData a)
                => a -> DataVec a -> DataVec a
scalarMulVec x ys = mapVec (* x) ys
