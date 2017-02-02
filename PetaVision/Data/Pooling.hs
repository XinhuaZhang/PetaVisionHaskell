{-# LANGUAGE FlexibleContexts #-}
module PetaVision.Data.Pooling
  ( PoolingType(..)
  , poolConduit
  , poolVecConduit
  , poolVecConduit1
  , poolArrayConduit
  , poolGrid
  , poolGridList
  , gridNum
  ) where

import           Control.DeepSeq
import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Array.Unboxed           as Arr
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           GHC.Float
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Array
import           PetaVision.Utility.Parallel
import           Prelude                      as P

data PoolingType
  = Max
  | Avg
  deriving (Show,Read)

{- CPU Pooling -}
sumPoolList
  :: Int -> a -> (a -> a -> a) -> (a -> a -> a) -> [a] -> [a]
sumPoolList poolSize zero add sub xs =
  L.scanl' (\c (d,e) -> add (sub c d) e)
           (L.foldl' add zero as)
           (P.zip xs bs)
  where (as,bs) = P.splitAt poolSize xs

maxPoolList :: (Ord a)
            => Int -> ([a] -> a) -> [a] -> [a]
maxPoolList poolSize maxOp ys@(x:xs)
  | P.length as == poolSize = max : maxPoolList poolSize maxOp xs
  | otherwise = [max]
  where (as,bs) = P.splitAt poolSize ys
        max = maxOp as

listOp :: (a -> a -> a) -> [a] -> [a] -> [a]
listOp op xs ys = P.zipWith op xs ys

avgPoolMatrix :: (Floating a)
              => Int -> [[a]] -> [[a]]
avgPoolMatrix poolSize xs =
  P.map (P.map (\y -> y / ((P.fromIntegral poolSize) ^ 2)) .
         sumPoolList poolSize 0 (+) (-)) $
  sumPoolList poolSize
              (P.repeat 0)
              (listOp (+))
              (listOp (-))
              xs

maxPoolMatrix :: (Ord a)
              => Int -> [[a]] -> [[a]]
maxPoolMatrix poolSize xs =
  P.map (maxPoolList poolSize P.maximum) $
  maxPoolList
    poolSize
    --(P.map P.maximum . L.transpose)
    (L.foldl1' (P.zipWith max))
    xs

pool :: (Floating a
        ,Ord a)
     => PoolingType -> Int -> [[a]] -> [[a]]
pool Max = maxPoolMatrix
pool Avg = avgPoolMatrix


-- CPU pooling via unboxed vector
sumPoolVecU :: (Unbox a)
            => Int
            -> (a -> a -> a)
            -> (a -> a -> a)
            -> VU.Vector a
            -> VU.Vector a
sumPoolVecU poolSize add sub xs =
  VU.scanl' (\c (d,e) -> add (sub c d) e)
            (VU.foldl1' add as)
            (VU.zip xs bs)
  where (as,bs) = VU.splitAt poolSize xs

sumPoolVec
  :: Int -> (a -> a -> a) -> (a -> a -> a) -> V.Vector a -> V.Vector a
sumPoolVec poolSize add sub xs =
  V.scanl' (\c (d,e) -> add (sub c d) e)
           (V.foldl1' add as)
           (V.zip xs bs)
  where (as,bs) = V.splitAt poolSize xs

maxPoolVecU
  :: (Ord a
     ,Unbox a)
  => Int -> (VU.Vector a -> a) -> VU.Vector a ->  [a]
maxPoolVecU poolSize maxOp ys
  | VU.length as == poolSize =
    max :
    maxPoolVecU poolSize
                maxOp
                (VU.tail ys)
  | otherwise = [max]
  where (as,bs) = VU.splitAt poolSize ys
        max = maxOp as

maxPoolVec
  :: (Ord a
     )
  => Int -> (V.Vector a -> a) -> V.Vector a ->  [a]
maxPoolVec poolSize maxOp ys
  | V.length as == poolSize =
    max :
    maxPoolVec poolSize
               maxOp
               (V.tail ys)
  | otherwise = [max]
  where (as,bs) = V.splitAt poolSize ys
        max = maxOp as

vecUOp
  :: (Unbox a)
  => (a -> a -> a) -> VU.Vector a -> VU.Vector a -> VU.Vector a
vecUOp op xs ys = VU.zipWith op xs ys

avgPoolVecMatrix
  :: (Floating a
     ,Unbox a)
  => Int -> V.Vector (VU.Vector a) -> V.Vector (VU.Vector a)
avgPoolVecMatrix poolSize xs =
  V.map (VU.map (\y -> y / ((P.fromIntegral poolSize) ^ 2)) .
         sumPoolVecU poolSize (+) (-)) $
  sumPoolVec poolSize
             (vecUOp (+))
             (vecUOp (-))
             xs

maxPoolVecMatrix
  :: (Ord a
     ,Unbox a)
  => Int -> V.Vector (VU.Vector a) -> [[a]]
maxPoolVecMatrix poolSize xs =
  P.map (maxPoolVecU poolSize VU.maximum) $
  maxPoolVec poolSize
             (V.foldl1' (VU.zipWith max))
             xs

poolVec
  :: (Floating a
     ,Ord a
     ,Unbox a)
  => PoolingType -> Int -> V.Vector (VU.Vector a) -> [VU.Vector a]
poolVec Max poolSize = P.map VU.fromList . maxPoolVecMatrix poolSize
poolVec Avg poolSize = V.toList . avgPoolVecMatrix poolSize

splitVector
  :: (Unbox a)
  => Int -> VU.Vector a -> [VU.Vector a]
splitVector n vec
  | VU.null vec = []
  | otherwise = as : splitVector n bs
  where (as,bs) = VU.splitAt n vec

extractSclice
  :: Arr.Array (Int,Int,Int) Double -> Int -> [[Double]]
extractSclice arr featureIndex =
  P.map (\y ->
           P.map (\x -> arr Arr.! (x,y,featureIndex))
                 [0 .. nx])
        [0 .. ny]
  where ((_,_,_),(ny,nx,_nf)) = bounds arr

sparse2NonSparse
  :: PVPDimension -> [(Int,Double)] -> [[[Double]]]
sparse2NonSparse (PVPDimension nx ny nf) frame =
  P.map (extractSclice arr)
        [0 .. nf - 1]
  where arr =
          accumArray (+)
                     0
                     ((0,0,0),(ny - 1,nx - 1,nf - 1)) $
          P.map (\(i,v) -> (indexMapping i,v)) frame :: Arr.Array (Int,Int,Int) Double
        indexMapping :: Int -> (Int,Int,Int)
        indexMapping i = (c,b,a)
          where n1 = nf * nx
                n2 = nf
                c = div i n1
                n3 = (mod i n1)
                b = div n3 n2
                a = mod n3 n2

extractScliceVec :: Arr.Array (Int,Int,Int) Double
                 -> Int
                 -> V.Vector (VU.Vector Double)
extractScliceVec arr featureIndex =
  V.map (\y ->
           VU.map (\x -> arr Arr.! (x,y,featureIndex)) $
           VU.generate (nx' + 1)
                       id) $
  V.generate (ny' + 1)
             id
  where ((_,_,_),(ny',nx',_nf)) = bounds arr


sparse2NonSparseVec
  :: PVPDimension -> [(Int,Double)] -> [V.Vector (VU.Vector Double)]
sparse2NonSparseVec (PVPDimension nx ny nf) frame =
  P.map (extractScliceVec arr)
        [0 .. nf - 1]
  where arr =
          accumArray (+)
                     0
                     ((0,0,0),(ny - 1,nx - 1,nf - 1)) $
          P.map (\(i,v) -> (indexMapping i,v)) frame :: Arr.Array (Int,Int,Int) Double
        indexMapping :: Int -> (Int,Int,Int)
        indexMapping i = (c,b,a)
          where n1 = nf * nx
                n2 = nf
                c = div i n1
                n3 = (mod i n1)
                b = div n3 n2
                a = mod n3 n2

poolConduit
  :: ParallelParams
  -> PoolingType
  -> Int
  -> Int
  -> Conduit PVPOutputData IO (VU.Vector (Int,Double))
poolConduit parallelParams poolingType poolingSize offset =
  do xs <- CL.take (batchSize parallelParams)
     if P.length xs > 0
        then do let pooledData =
                      case P.head xs of
                        PVP_OUTPUT_ACT _ _ ->
                          error "Dosen't support pooling PVP_OUTPUT_ACT."
                        PVP_OUTPUT_NONSPIKING_ACT _ _ ->
                          parMapChunk
                            parallelParams
                            rdeepseq
                            (\(PVP_OUTPUT_NONSPIKING_ACT (PVPDimension nx' ny' nf') x) ->
                               let arr =
                                     listArray ((0,0,0)
                                               ,(ny' - 1,nx' - 1,nf' - 1)) .
                                     VU.toList $
                                     x
                               in VU.filter (\(i,v) -> v /= 0) .
                                  (\vec ->
                                     VU.zip (VU.generate (VU.length vec)
                                                         (\i -> i + 1 + offset))
                                            vec) .
                                  VU.fromList .
                                  P.concatMap P.concat .
                                  P.map (pool poolingType poolingSize .
                                         extractSclice arr) $
                                  [0 .. nf' - 1])
                            xs
                        PVP_OUTPUT_ACT_SPARSEVALUES _ _ ->
                          parMapChunk
                            parallelParams
                            rdeepseq
                            (\(PVP_OUTPUT_ACT_SPARSEVALUES layout x) ->
                               VU.filter (\(i,v) -> v /= 0) .
                               (\vec ->
                                  VU.zip (VU.generate (VU.length vec)
                                                      (\i -> i + 1 + offset))
                                         vec) .
                               VU.fromList .
                               P.concatMap P.concat .
                               P.map (pool poolingType poolingSize) .
                               sparse2NonSparse layout . VU.toList $
                               x)
                            xs
                sourceList $!! pooledData
                poolConduit parallelParams poolingType poolingSize offset
        else return ()


poolVecConduit
  :: ParallelParams
  -> PoolingType
  -> Int
  -> Int
  -> Conduit PVPOutputData (ResourceT IO) (VU.Vector (Int,Double))
poolVecConduit parallelParams poolingType poolingSize offset =
  do xs <- CL.take (batchSize parallelParams)
     if P.length xs > 0
        then do let pooledData =
                      case P.head xs of
                        PVP_OUTPUT_ACT _ _ ->
                          error "Dosen't support pooling PVP_OUTPUT_ACT."
                        PVP_OUTPUT_NONSPIKING_ACT _ _ ->
                          parMapChunk
                            parallelParams
                            rdeepseq
                            (\(PVP_OUTPUT_NONSPIKING_ACT (PVPDimension nx' ny' nf') x) ->
                               let arr =
                                     listArray ((0,0,0)
                                               ,(ny' - 1,nx' - 1,nf' - 1)) .
                                     VU.toList $
                                     x
                               in VU.filter (\(i,v) -> v /= 0) .
                                  (\vec ->
                                     VU.zip (VU.generate (VU.length vec)
                                                         (\i -> i + 1 + offset))
                                            vec) .
                                  VU.concat .
                                  P.concat .
                                  P.map (poolVec poolingType poolingSize .
                                         extractScliceVec arr) $
                                  [0 .. nf' - 1])
                            xs
                        PVP_OUTPUT_ACT_SPARSEVALUES _ _ ->
                          parMapChunk
                            parallelParams
                            rdeepseq
                            (\(PVP_OUTPUT_ACT_SPARSEVALUES layout x) ->
                               VU.filter (\(i,v) -> v /= 0) .
                               (\vec ->
                                  VU.zip (VU.generate (VU.length vec)
                                                      (\i -> i + 1 + offset))
                                         vec) .
                               VU.concat .
                               P.concat .
                               P.map (poolVec poolingType poolingSize) .
                               sparse2NonSparseVec layout . VU.toList $
                               x)
                            xs
                sourceList $!! pooledData
                poolVecConduit parallelParams poolingType poolingSize offset
        else return ()
        

poolVecConduit1
  :: ParallelParams
  -> PoolingType
  -> Int
  -> Int
  -> Conduit PVPOutputData (ResourceT IO) (VU.Vector (Int,Double))
poolVecConduit1 parallelParams poolingType poolingSize offset =
  do xs <- CL.take (batchSize parallelParams)
     if P.length xs > 0
        then do let pooledData =
                      case P.head xs of
                        PVP_OUTPUT_ACT _ _ ->
                          error "Dosen't support pooling PVP_OUTPUT_ACT."
                        PVP_OUTPUT_NONSPIKING_ACT _ _ ->
                          parMapChunk
                            parallelParams
                            rdeepseq
                            (\(PVP_OUTPUT_NONSPIKING_ACT (PVPDimension nx' ny' nf') x) ->
                               let arr =
                                     listArray ((0,0,0)
                                               ,(ny' - 1,nx' - 1,nf' - 1)) .
                                     VU.toList $
                                     x
                                   pooledList =
                                     L.map (poolVec poolingType poolingSize .
                                            extractScliceVec arr)
                                           [0 .. nf' - 1]
                                   newNy = L.length . L.head $ pooledList
                                   newNx =
                                     VU.length . L.head . L.head $ pooledList
                                   result =
                                     computeS .
                                     downsample [4,4,1] .
                                     fromUnboxed (Z :. nf' :. newNy :. newNx) .
                                     VU.concat . L.concat $
                                     pooledList
                               in VU.filter (\(i,v) -> v /= 0) .
                                  (\vec ->
                                     VU.zip (VU.generate (VU.length vec)
                                                         (\i -> i + 1 + offset))
                                            vec) .
                                  toUnboxed $
                                  result)
                            xs
                        PVP_OUTPUT_ACT_SPARSEVALUES _ _ ->
                          parMapChunk
                            parallelParams
                            rdeepseq
                            (\(PVP_OUTPUT_ACT_SPARSEVALUES layout@(PVPDimension nx' ny' nf') x) ->
                               let pooledList =
                                     P.map (poolVec poolingType poolingSize) .
                                     sparse2NonSparseVec layout . VU.toList $
                                     x
                                   newNy = L.length . L.head $ pooledList
                                   newNx =
                                     VU.length . L.head . L.head $ pooledList
                                   result =
                                     computeS .
                                     downsample [4,4,1] .
                                     fromUnboxed (Z :. nf' :. newNy :. newNx) .
                                     VU.concat . L.concat $
                                     pooledList
                               in VU.filter (\(i,v) -> v /= 0) .
                                  (\vec ->
                                     VU.zip (VU.generate (VU.length vec)
                                                         (\i -> i + 1 + offset))
                                            vec) .
                                  toUnboxed $
                                  result)
                            xs
                sourceList $!! pooledData
                poolVecConduit1 parallelParams poolingType poolingSize offset
        else return ()


poolArrayConduit
  :: ParallelParams
  -> PoolingType
  -> Int
  -> Int
  -> Conduit PVPOutputData (ResourceT IO) (R.Array U DIM3 Double)
poolArrayConduit parallelParams poolingType poolingSize offset = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let pooledData =
              case P.head xs of
                PVP_OUTPUT_ACT _ _ ->
                  error "Dosen't support pooling PVP_OUTPUT_ACT."
                PVP_OUTPUT_NONSPIKING_ACT _ _ ->
                  parMapChunk
                    parallelParams
                    rseq
                    (\(PVP_OUTPUT_NONSPIKING_ACT (PVPDimension nx' ny' nf') x) ->
                        let arr =
                              listArray ((0, 0, 0), (ny' - 1, nx' - 1, nf' - 1)) .
                              VU.toList $
                              x
                            pooledList =
                              L.map
                                (poolVec poolingType poolingSize .
                                 extractScliceVec arr)
                                [0 .. nf' - 1]
                        in extractFeaturePoint nf' pooledList)
                    xs
                PVP_OUTPUT_ACT_SPARSEVALUES _ _ ->
                  parMapChunk
                    parallelParams
                    rseq
                    (\(PVP_OUTPUT_ACT_SPARSEVALUES layout@(PVPDimension _nx' _ny' nf') x) ->
                        extractFeaturePoint nf' .
                        P.map (poolVec poolingType poolingSize) .
                        sparse2NonSparseVec layout . VU.toList $
                        x)
                    xs
                _ -> error "poolArrayConduit: pvp file format is supported."
        sourceList pooledData
        poolArrayConduit parallelParams poolingType poolingSize offset)
  where
    extractFeaturePoint nf' pooledList' =
      let newNy = L.length . L.head $ pooledList'
          newNx = VU.length . L.head . L.head $ pooledList'
          result =
            computeS .
            R.backpermute
              (Z :. newNy :. newNx :. nf')
              (\(Z :. j :. i :. k) -> (Z :. k :. j :. i)) .
            fromUnboxed (Z :. nf' :. newNy :. newNx) . VU.concat . L.concat $
            pooledList'
      in deepSeqArray result result
      
{-# INLINE poolGrid #-}

poolGridList
  :: (R.Source s e, Unbox e)
  => Int
  -> Int
  -> (R.Array D DIM3 e -> VU.Vector e)
  -> R.Array s DIM3 e
  -> [VU.Vector e]
poolGridList poolSize stride f arr =
  [ f . cropUnsafe [0, i, j] [nf', poolSize, poolSize] $ arr
  | i <- startPointList nx'
  , j <- startPointList ny' ]
  where
    (Z :. ny' :. nx' :. nf') = extent arr
    startPointList len =
      L.filter (\i -> i + poolSize <= len) [0,stride .. len - 1]

poolGrid
  :: (R.Source s e, Unbox e)
  => Int
  -> Int
  -> (R.Array D DIM3 e -> VU.Vector e)
  -> R.Array s DIM3 e
  -> VU.Vector e
poolGrid poolSize stride f = VU.concat . poolGridList poolSize stride f 


gridNum :: Int -> Int -> Int -> Int -> Int
gridNum poolSize stride nx' ny' =
  L.length (startPointList nx') * L.length (startPointList ny')
  where
    startPointList len =
      L.filter (\i -> i + poolSize <= len) [0,stride .. len - 1]
