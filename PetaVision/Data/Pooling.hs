module PetaVision.Data.Pooling
  (module CUDA.DataType
  ,PoolingType(..)
  ,poolAccConduit
  ,poolConduit
  ,poolVecConduit)
  where

import           Control.DeepSeq
import           Control.DeepSeq
import           Control.Monad               as M
import           Control.Monad.IO.Class      (liftIO)
import           CUDA.DataType
import           CUDA.MultiGPU
import           Data.Array.Accelerate       as A
import           Data.Array.Accelerate.CUDA  as A
import           Data.Array.Unboxed          as Arr
import           Data.Conduit
import           Data.Conduit.List           as CL
import           Data.List                   as L
import           Data.Maybe                  as Maybe
import           Data.Vector         as V
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
                    => PVPDimension
                    -> Acc (A.Vector (Int,a))
                    -> Acc (A.Array DIM3 a)
sparse2NonsparseAcc (PVPDimension nx ny nf) pairAcc =
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
  -> Int
  -> Conduit PVPOutputData IO (VU.Vector (Int,Double))
poolAccConduit GPUFloat ctx poolingType batchSize offset =
  do xs <- M.replicateM batchSize await
     let batch = Maybe.catMaybes xs
     if P.length batch > 0
        then do let (PVPDimension nx' ny' nf') =
                      case P.head batch of
                        PVP_OUTPUT_ACT _ _ ->
                          error "Dosen't support pooling PVP_OUTPUT_ACT"
                        PVP_OUTPUT_NONSPIKING_ACT d _ -> d
                        PVP_OUTPUT_ACT_SPARSEVALUES d _ -> d
                    indVec =
                      A.generate
                        (A.index1 $ A.constant (nx' * ny' * nf'))
                        (\ix ->
                           let (Z :. i) =
                                 A.unlift ix :: ((A.:.) A.Z (A.Exp Int))
                           in (i + 1 + A.constant offset) :: A.Exp Int)
                    pooledData =
                      case P.head batch of
                        PVP_OUTPUT_ACT _ _ ->
                          error "Dosen't support pooling PVP_OUTPUT_ACT"
                        PVP_OUTPUT_NONSPIKING_ACT _ _ ->
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
                          P.map (\(PVP_OUTPUT_NONSPIKING_ACT _ x) ->
                                   A.fromList (Z :. ny' :. nx' :. nf') .
                                   VU.toList . VU.map double2Float $
                                   x)
                                batch :: [A.Array DIM1 (Int,Float)]
                        PVP_OUTPUT_ACT_SPARSEVALUES layout@(PVPDimension nx ny nf) _ ->
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
                          P.map (\(PVP_OUTPUT_ACT_SPARSEVALUES _ xs) ->
                                   A.fromList (Z :. VU.length xs) .
                                   VU.toList .
                                   VU.map (\(i,x) -> (i,double2Float x)) $
                                   xs)
                                batch :: [A.Vector (Int,Float)]
                CL.sourceList $!!
                  P.map (VU.fromList .
                         (P.map (\(j,x) -> (j,float2Double x))) . A.toList)
                        pooledData
                poolAccConduit GPUFloat ctx poolingType batchSize offset
        else return ()
poolAccConduit GPUDouble ctx poolingType batchSize offset =
  do xs <- M.replicateM batchSize await
     let batch = Maybe.catMaybes xs
     if P.length batch > 0
        then do let (PVPDimension nx' ny' nf') =
                      case P.head batch of
                        PVP_OUTPUT_ACT _ _ ->
                          error "Dosen't support pooling PVP_OUTPUT_ACT"
                        PVP_OUTPUT_NONSPIKING_ACT d _ -> d
                        PVP_OUTPUT_ACT_SPARSEVALUES d _ -> d
                    indVec =
                      A.generate
                        (A.index1 $ A.constant (nx' * ny' * nf'))
                        (\ix ->
                           let (Z :. i) =
                                 A.unlift ix :: ((A.:.) A.Z (A.Exp Int))
                           in (i + 1 + A.constant offset) :: A.Exp Int)
                    pooledData =
                      case P.head batch of
                        PVP_OUTPUT_ACT _ _ ->
                          error "Dosen't support pooling PVP_OUTPUT_ACT"
                        PVP_OUTPUT_NONSPIKING_ACT _ _ ->
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
                          P.map (\(PVP_OUTPUT_NONSPIKING_ACT _ x) ->
                                   A.fromList (Z :. ny' :. nx' :. nf') .
                                   VU.toList $
                                   x)
                                batch :: [A.Array DIM1 (Int,Double)]
                        PVP_OUTPUT_ACT_SPARSEVALUES layout _ ->
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
                          P.map (\(PVP_OUTPUT_ACT_SPARSEVALUES _ xs) ->
                                   A.fromList (Z :. VU.length xs) . VU.toList $
                                   xs)
                                batch :: [A.Vector (Int,Double)]
                CL.sourceList $!! (P.map (VU.fromList . A.toList) pooledData)
                poolAccConduit GPUDouble ctx poolingType batchSize offset
        else return ()

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
  V.map (\y -> VU.map (\x -> arr Arr.! (x,y,featureIndex)) $ VU.generate nx id) $
  V.generate ny id
  where ((_,_,_),(ny,nx,_nf)) = bounds arr
  

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
  -> Conduit PVPOutputData IO (VU.Vector (Int,Double))
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
                poolConduit parallelParams poolingType poolingSize offset
        else return ()
