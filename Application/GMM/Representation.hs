{-# LANGUAGE DeriveGeneric #-}
module Application.GMM.Representation
  (DataVec
  ,fromListDense
  ,fromListSparse
  ,toListDense
  ,toListSparse
  ,dense2Sparse
  ,sparse2Dense
  ,merge
  ,sumVec
  ,productVec
  ,mapVec
  ,findVec
  ,lengthVec)
  where

import           Control.DeepSeq
import           Data.Binary
import           Data.List           as L
import           Data.Vector.Unboxed as VU
import           GHC.Generics
import Data.Maybe

data DataVec a
  = DenseVec !(Vector a)
  | SparseVec ![(Int,a)]
  deriving (Show,Generic)

instance (Binary a
         ,Unbox a) =>
         Binary (DataVec a) where
  put (DenseVec vec) =
    do putWord8 0
       put $ VU.toList vec
  put (SparseVec xs) =
    do putWord8 1
       put xs
  get =
    do t <- getWord8
       case t of
         0 ->
           do xs <- get
              return $ DenseVec . VU.fromList $ xs
         1 ->
           do xs <- get
              return $ SparseVec xs
              
instance (NFData a) =>
         NFData (DataVec a) where
  rnf (DenseVec vec) = rnf vec
  rnf (SparseVec xs) = rnf xs


fromListDense :: (Unbox a)
              => [a] -> DataVec a
fromListDense = DenseVec . VU.fromList

fromListSparse :: [(Int,a)] -> DataVec a
fromListSparse = SparseVec . L.sortOn fst


toListDense :: (Unbox a
               ,Num a)
            => DataVec a -> [a]
toListDense (DenseVec vec) = VU.toList vec
toListDense (SparseVec xs) =
  VU.toList .
  VU.accum (+)
           (VU.replicate (L.length xs)
                         0) $
  xs

toListSparse :: (Unbox a
                ,Num a)
             => DataVec a -> [(Int,a)]
toListSparse (DenseVec vec) =
  VU.toList $
  VU.zip (VU.generate (VU.length vec)
                      id)
         vec
toListSparse (SparseVec xs) = xs

dense2Sparse :: (Unbox a)
             => DataVec a -> DataVec a
dense2Sparse (DenseVec vec) =
  SparseVec . VU.toList $
  VU.zip (VU.generate (VU.length vec)
                      id)
         vec
dense2Sparse _ = error "Input must be a dense vector."

sparse2Dense :: (Unbox a
                ,Num a)
             => DataVec a -> DataVec a
sparse2Dense (SparseVec xs) =
  DenseVec $
  accum (+)
        (VU.replicate (L.length xs)
                      0)
        xs
sparse2Dense _ = error "Input must be a sparse vector."

mergeSparse
  :: (Num a)
  => (a -> a -> a) -> [(Int,a)] -> [(Int,a)] -> [(Int,a)]
mergeSparse _ xs [] = xs
mergeSparse _ [] ys = ys
mergeSparse f ((ix,x):xs) ((iy,y):ys)
  | ix == iy = (ix,f x y) : mergeSparse f xs ys
  | ix < iy =
    (ix,f x 0) :
    mergeSparse f
                xs
                ((iy,y) : ys)
  | otherwise =
    (iy,f 0 y) :
    mergeSparse f
                ((ix,x) : xs)
                ys

merge
  :: (Unbox a
     ,Num a)
  => (a -> a -> a) -> DataVec a -> DataVec a -> DataVec a
merge f (DenseVec d1) (DenseVec d2) = DenseVec $ VU.zipWith f d1 d2
merge f (SparseVec s1) (SparseVec s2) = SparseVec $ mergeSparse f s1 s2
merge f (DenseVec d1) (SparseVec s2) = DenseVec $ VU.accum f d1 s2
merge f (SparseVec s1) (DenseVec d2) =
  merge f
        (DenseVec d2)
        (SparseVec s1)

sumVec :: (Num a
       ,Unbox a)
    => DataVec a -> a
sumVec (DenseVec vec) = VU.sum vec
sumVec (SparseVec xs) = L.foldl1' (+) . L.map snd $ xs

productVec :: (Num a
        ,Unbox a)
     => DataVec a -> a
productVec (DenseVec vec) = VU.product vec
productVec (SparseVec xs) = L.foldl1' (*) . L.map snd $ xs

mapVec :: (Unbox a)
       => (a -> a) -> DataVec a -> DataVec a
mapVec f (DenseVec vec) = DenseVec $ VU.map f vec
mapVec f (SparseVec xs) = SparseVec $ L.map (\(i,x) -> (i,f x)) xs

findVec :: (Unbox a) => (a -> Bool) -> DataVec a -> Maybe a
findVec f (DenseVec vec) = VU.find f vec
findVec f (SparseVec xs) =
  case L.find (f . snd) xs of
    Nothing -> Nothing
    Just x -> Just $ snd x

lengthVec :: (Unbox a) => DataVec a -> Int
lengthVec (DenseVec vec) = VU.length vec
lengthVec (SparseVec xs) = L.length xs
