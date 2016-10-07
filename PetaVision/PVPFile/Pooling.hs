module PetaVision.PVPFile.Pooling
  (module CUDA.DataType
  ,PoolingType(..)
  ,poolConduit)
  where

import           Control.DeepSeq
import           Control.Monad              as M
import           Control.Monad.IO.Class     (liftIO)
import           CUDA.DataType
import           CUDA.MultiGPU
import           Data.Array.Accelerate      as A
import           Data.Array.Accelerate.CUDA as A
import           Data.Array.Unboxed         as AU
import           Data.Conduit
import           Data.Conduit.List          as CL
import           Data.Maybe                 as Maybe
import           GHC.Float
import           PetaVision.PVPFile.IO
import           Prelude                    as P

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

poolConduit
  :: GPUDataType
  -> [A.Context]
  -> PoolingType
  -> Int
  -> (Int,Int,Int)
  -> Conduit PVPOutputData IO (AU.Array (Int,Int,Int) Double)
poolConduit GPUFloat ctx poolingType batchSize layout@(ny,nx,nf) =
  do batch <- CL.take batchSize
     if P.length batch > 0
        then do let pooledData =
                      case P.head batch of
                        PVP_ACT _ -> error "Dosen't support pooling PVP_ACT"
                        PVP_NONSPIKING_ACT _ ->
                          multiGPUStream
                            ctx
                            (stencil (poolAcc poolingType)
                                     (Constant 0)) $
                          P.map (\(PVP_NONSPIKING_ACT x) ->
                                   A.fromList (Z :. ny :. nx :. nf) $
                                   P.map double2Float x)
                                batch :: [A.Array DIM3 Float]
                        PVP_ACT_SPARSEVALUES _ ->
                          multiGPUStream
                            ctx
                            (sparse2NonsparseAcc layout >->
                             stencil (poolAcc poolingType)
                                     (Constant 0)) $
                          P.map (\(PVP_ACT_SPARSEVALUES xs) ->
                                   A.fromList (Z :. P.length xs) $
                                   P.map (\(i,x) -> (i,double2Float x)) xs)
                                batch :: [A.Array DIM3 Float]
                CL.sourceList $!!
                  P.map (\arr ->
                           AU.array (bounds arr)
                                    (P.map (\(i,x) -> (i,float2Double x)) $
                                     assocs arr)) $
                  (P.map A.toIArray pooledData :: [AU.Array (Int,Int,Int) Float])
                liftIO $ performGCCtx ctx
                poolConduit GPUFloat ctx poolingType batchSize layout
        else return ()
poolConduit GPUDouble ctx poolingType batchSize layout@(ny,nx,nf) =
  do xs <- M.replicateM  batchSize await
     let batch = Maybe.catMaybes xs
     if P.length batch > 0
        then do let pooledData =
                      case P.head batch of
                        PVP_ACT _ -> error "Dosen't support pooling PVP_ACT"
                        PVP_NONSPIKING_ACT _ ->
                          multiGPUStream
                            ctx
                            (stencil (poolAcc poolingType)
                                     (Constant 0)) $
                          P.map (\(PVP_NONSPIKING_ACT x) ->
                                   A.fromList (Z :. ny :. nx :. nf)
                                              x)
                                batch :: [A.Array DIM3 Double]
                        PVP_ACT_SPARSEVALUES _ ->
                          multiGPUStream
                            ctx
                            (sparse2NonsparseAcc layout >->
                             stencil (poolAcc poolingType)
                                     (Constant 0)) $
                          P.map (\(PVP_ACT_SPARSEVALUES xs) ->
                                   A.fromList (Z :. P.length xs)
                                              xs)
                                batch :: [A.Array DIM3 Double]
                CL.sourceList $!!
                  (P.map A.toIArray pooledData :: [AU.Array (Int,Int,Int) Double])
                liftIO $ performGCCtx ctx
                poolConduit GPUFloat ctx poolingType batchSize layout
        else return ()
