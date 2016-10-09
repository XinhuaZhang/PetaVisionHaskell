module PetaVision.PVPFile.Pooling
  (module CUDA.DataType
  ,PoolingType(..)
  ,poolAccConduit
  ,poolConduit)
  where

import           Control.DeepSeq
import           Control.Monad               as M
import           Control.Monad.IO.Class      (liftIO)
import           CUDA.DataType
import           CUDA.MultiGPU
import           Data.Array                  as Arr
import           Data.Array.Accelerate       as A
import           Data.Array.Accelerate.CUDA  as A
import           Data.Conduit
import           Data.Conduit.List           as CL
import           Data.List                   as L
import           Data.Maybe                  as Maybe
import           Data.Vector.Unboxed         as VU
import           GHC.Float
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel
import           Prelude                     as P

data PoolingType
  = Max
  | Avg
  deriving (Show,Read)

sparse2NonsparseAcc :: (Elt a
                       ,IsFloating a)
                    => (Int,Int,Int)
                    -> Acc (A.Vector (Int,a))
                    -> Acc (A.Array DIM3 a)
sparse2NonsparseAcc (nf,nx,ny) pairAcc =
  A.reshape (A.lift (Z :. ny :. nx :. nf)) $ A.scatter ind d val
  where (ind,val) = A.unzip pairAcc
        size = nf * nx * ny
        d =
          A.generate (A.lift $ Z :. size)
                     (\ix -> A.constant 0)

-- avgPoolAcc :: (Elt a
--               ,IsFloating a)
--            => Stencil3x5x5 a -> Exp a
-- avgPoolAcc (a,b,c,d,e) = ((g a + g b + g c + g d + g e) / (A.constant 25))
--   where f :: (Elt a
--              ,IsFloating a)
--           => (Stencil3 a) -> Exp a
--         f (x1,x2,x3) = x2
--         g (y1,y2,y3,y4,y5) = (f y1) + (f y2) + (f y3) + (f y4) + (f y5)

avgPoolAcc :: (Elt a
              ,IsFloating a)
           => Stencil5x5x3 a -> Exp a
avgPoolAcc (a,b,c) = (g b / (A.constant 25))
  where f :: (Elt a
             ,IsFloating a)
          => (Stencil5 a) -> Exp a
        f (x1,x2,x3,x4,x5) = x1 + x2 + x3 + x4 + x5
        g (y1,y2,y3,y4,y5) = (f y1) + (f y2) + (f y3) + (f y4) + (f y5)

-- maxPoolAcc :: (Elt a
--               ,IsFloating a
--               ,IsScalar a)
--            => Stencil3x5x5 a -> Exp a
-- maxPoolAcc (a,b,c,d,e) = P.maximum [g a,g b,g c,g d,g e]
--   where f :: (Elt a
--              ,IsFloating a)
--           => (Stencil3 a) -> Exp a
--         f (x1,x2,x3) = x2
--         g (y1,y2,y3,y4,y5) = P.maximum [(f y1),(f y2),(f y3),(f y4),(f y5)]

maxPoolAcc :: (Elt a
              ,IsFloating a
              ,IsScalar a)
           => Stencil5x5x3 a -> Exp a
maxPoolAcc (a,b,c) = g b
  where f :: (Elt a
             ,IsFloating a)
          => (Stencil5 a) -> Exp a
        f (x1,x2,x3,x4,x5) = P.maximum [x1,x2,x3,x4,x5]
        g (y1,y2,y3,y4,y5) = P.maximum [(f y1),(f y2),(f y3),(f y4),(f y5)]


poolAcc :: (Elt a
           ,IsFloating a
           ,IsScalar a)
        => PoolingType -> Stencil5x5x3 a -> Exp a
poolAcc Max = maxPoolAcc
poolAcc Avg = avgPoolAcc

poolAccConduit
  :: GPUDataType
  -> [A.Context]
  -> PoolingType
  -> Int
  -> (Int,Int,Int)
  -> Int
  -> Conduit PVPOutputData IO (VU.Vector (Int,Double))
poolAccConduit GPUFloat ctx poolingType batchSize layout@(ny,nx,nf) offset =
  do xs <- M.replicateM batchSize await
     let batch = Maybe.catMaybes xs
     if P.length batch > 0
        then do let indVec =
                      A.generate
                        (A.index1 $ A.constant (nx * ny * nf))
                        (\ix ->
                           let (Z :. i) =
                                 A.unlift ix :: ((A.:.) A.Z (A.Exp Int))
                           in (i + 1 + A.constant offset) :: A.Exp Int)
                    pooledData =
                      case P.head batch of
                        PVP_ACT _ -> error "Dosen't support pooling PVP_ACT"
                        PVP_NONSPIKING_ACT _ ->
                          multiGPUStream
                            ctx
                            (stencil (poolAcc poolingType)
                                     (Constant 0) >->
                             A.flatten >->
                             (A.zip indVec) >->
                             (A.filter (\x ->
                                          let (i,v) =
                                                A.unlift x :: (A.Exp Int
                                                              ,A.Exp Float)
                                          in v A.>* (A.constant 0)))) $
                          P.map (\(PVP_NONSPIKING_ACT x) ->
                                   A.fromList (Z :. ny :. nx :. nf) $
                                   P.map double2Float x)
                                batch :: [A.Array DIM1 (Int,Float)]
                        PVP_ACT_SPARSEVALUES _ ->
                          multiGPUStream
                            ctx
                            (sparse2NonsparseAcc layout >->
                             stencil (poolAcc poolingType)
                                     (Constant 0) >->
                             A.flatten >->
                             (A.zip indVec) >->
                             (A.filter (\x ->
                                          let (i,v) =
                                                A.unlift x :: (A.Exp Int
                                                              ,A.Exp Float)
                                          in v A.>* (A.constant 0)))) $
                          P.map (\(PVP_ACT_SPARSEVALUES xs) ->
                                   A.fromList (Z :. P.length xs) $
                                   P.map (\(i,x) -> (i,double2Float x)) $ xs)
                                batch :: [A.Vector (Int,Float)]
                CL.sourceList $!!
                  P.map (VU.fromList .
                         (P.map (\(j,x) -> (j,float2Double x))) . A.toList)
                        pooledData
                poolAccConduit GPUFloat ctx poolingType batchSize layout offset
        else return ()
poolAccConduit GPUDouble ctx poolingType batchSize layout@(ny,nx,nf) offset =
  do xs <- M.replicateM batchSize await
     let batch = Maybe.catMaybes xs
     if P.length batch > 0
        then do let indVec =
                      A.generate
                        (A.index1 $ A.constant (nx * ny * nf))
                        (\ix ->
                           let (Z :. i) =
                                 A.unlift ix :: ((A.:.) A.Z (A.Exp Int))
                           in (i + 1 + A.constant offset) :: A.Exp Int)
                    pooledData =
                      case P.head batch of
                        PVP_ACT _ -> error "Dosen't support pooling PVP_ACT"
                        PVP_NONSPIKING_ACT _ ->
                          multiGPUStream
                            ctx
                            (stencil (poolAcc poolingType)
                                     (Constant 0) >->
                             A.flatten >->
                             (A.zip indVec) >->
                             (A.filter (\x ->
                                          let (i,v) =
                                                A.unlift x :: (A.Exp Int
                                                              ,A.Exp Double)
                                          in v A.>* (A.constant 0)))) $
                          P.map (\(PVP_NONSPIKING_ACT x) ->
                                   A.fromList (Z :. ny :. nx :. nf)
                                              x)
                                batch :: [A.Array DIM1 (Int,Double)]
                        PVP_ACT_SPARSEVALUES _ ->
                          multiGPUStream
                            ctx
                            (sparse2NonsparseAcc layout >->
                             stencil (poolAcc poolingType)
                                     (Constant 0) >->
                             A.flatten >->
                             (A.zip indVec) >->
                             (A.filter (\x ->
                                          let (i,v) =
                                                A.unlift x :: (A.Exp Int
                                                              ,A.Exp Double)
                                          in v A.>* (A.constant 0)))) $
                          P.map (\(PVP_ACT_SPARSEVALUES xs) ->
                                   A.fromList (Z :. P.length xs)
                                              xs)
                                batch :: [A.Vector (Int,Double)]
                CL.sourceList $!! (P.map (VU.fromList . A.toList) pooledData)
                poolAccConduit GPUDouble ctx poolingType batchSize layout offset
        else return ()


{- CPU Pooling -}
sumPoolList
  :: Int -> a -> (a -> a -> a) -> (a -> a -> a) -> [a] -> [a]
sumPoolList poolSize zero add sub xs =
  L.scanl' (\c (d,e) -> add (sub c d) e)
           (L.foldl' add zero as)
           (P.zip xs bs)
  where (as,bs) = P.splitAt poolSize xs
  
sumPoolVec
  :: (Unbox a) => Int -> a -> (a -> a -> a) -> (a -> a -> a) -> VU.Vector a -> VU.Vector a
sumPoolVec poolSize zero add sub xs =
  VU.scanl' (\c (d,e) -> add (sub c d) e)
            (VU.foldl' add zero as)
            (VU.zip xs bs)
  where (as,bs) = VU.splitAt poolSize xs

maxPoolList :: (Ord a)
            => Int -> ([a] -> a) -> [a] -> [a]
maxPoolList poolSize maxOp ys@(x:xs)
  | P.length as == poolSize = max : maxPoolList poolSize maxOp xs
  | otherwise = [max]
  where (as,bs) = P.splitAt poolSize ys
        max = maxOp as

listOp :: (a -> a -> a) -> [a] -> [a] -> [a]
listOp op xs ys = P.zipWith op xs ys

vecOp
  :: (Unbox a)
  => (a -> a -> a) -> VU.Vector a -> VU.Vector a -> VU.Vector a
vecOp op xs ys = VU.zipWith op xs ys

avgPoolMatrix :: (Floating a, Unbox a)
              => Int -> [VU.Vector a] -> [VU.Vector a]
avgPoolMatrix poolSize xs =
  P.map (VU.map (\y -> y / ((P.fromIntegral poolSize) ^ 2)) .
         sumPoolVec poolSize 0 (+) (-)) $
  sumPoolList poolSize
              (VU.replicate (VU.length $ P.head xs) 0)
              (vecOp (+))
              (vecOp (-))
              xs

maxPoolMatrix :: (Ord a)
              => Int -> [[a]] -> [[a]]
maxPoolMatrix poolSize xs =
  P.map (maxPoolList poolSize P.maximum) $
  maxPoolList poolSize
              (P.map P.maximum . L.transpose)
              xs
              

pool :: (Floating a,Ord a,Unbox a)
     => PoolingType -> Int -> [VU.Vector a] -> [VU.Vector a]
--pool Max = maxPoolMatrix
pool Avg = avgPoolMatrix

splitVector
  :: (Unbox a)
  => Int -> VU.Vector a -> [VU.Vector a]
splitVector n vec
 | VU.null vec = []
 | otherwise   = as : splitVector n bs
  where (as,bs) = VU.splitAt n vec

sparse2NonSparse
  :: (Int,Int,Int) -> [(Int,Double)] -> [[VU.Vector Double]]
sparse2NonSparse (ny,nx,nf) frame =
  P.map (splitVector ny) . splitVector (nx * nf) . VU.fromList . elems $ arr
  where arr =
          accumArray (+)
                     0
                     ((0,0,0),(nf - 1,nx - 1,ny - 1)) .
          P.map (\(i,v) -> (indexMapping i,v)) $
          frame
        indexMapping :: Int -> (Int,Int,Int)
        indexMapping i = (a,b,c)
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
  -> (Int,Int,Int)
  -> Int
  -> Conduit PVPOutputData IO (VU.Vector (Int,Double))
poolConduit parallelParams poolingType poolingSize layout@(ny,nx,nf) offset =
  do xs <- CL.take (batchSize parallelParams)
     if P.length xs > 0
        then do let pooledData =
                      case P.head xs of
                        PVP_ACT _ -> error "Dosen't support pooling PVP_ACT."
                        PVP_NONSPIKING_ACT _ ->
                          parMapChunk
                            parallelParams
                            rdeepseq
                            (\(PVP_NONSPIKING_ACT x) ->
                               VU.filter (\(i,v) -> v /= 0) .
                               (\vec ->
                                  VU.zip (VU.generate (VU.length vec)
                                                      (\i -> i + 1 + offset))
                                         vec) .
                               VU.concat .
                               P.map (VU.concat .
                                      pool poolingType poolingSize .
                                      splitVector nx . VU.fromList) .
                               L.transpose .
                               P.map VU.toList . splitVector nf . VU.fromList $
                               x)
                            xs
                        PVP_ACT_SPARSEVALUES _ ->
                          parMapChunk
                            parallelParams
                            rdeepseq
                            (\(PVP_ACT_SPARSEVALUES x) ->
                               VU.filter (\(i,v) -> v /= 0) .
                               (VU.zip (VU.generate (nx * ny * nf)
                                                    (\i -> i + 1 + offset))) .
                               VU.concat .
                               P.map (VU.concat . pool poolingType poolingSize) .
                               sparse2NonSparse layout $
                               x)
                            xs
                sourceList pooledData
                poolConduit parallelParams poolingType poolingSize layout offset
        else return ()
