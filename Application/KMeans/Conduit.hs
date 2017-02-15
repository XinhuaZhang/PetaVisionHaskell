{-# LANGUAGE FlexibleContexts #-}
module Application.KMeans.Conduit where

import           AI.Clustering.KMeans
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Matrix.Unboxed          as MU
import           Data.Vector.Unboxed          as VU
import           PetaVision.Utility.Array

kmeansArrSink
  :: (R.Source s Double)
  => FilePath
  -> Int
  -> Int
  -> Int
  -> Sink (Array s DIM3 Double) (ResourceT IO) (KMeans (Vector Double))
kmeansArrSink filePath k numTrain downsampleFactor = do
  xs <- CL.take numTrain
  let ys =
        L.concatMap
          (extractFeaturePoint .
           downsample [downsampleFactor, downsampleFactor, 1])
          xs
      model =
        kmeans
          k
          (MU.fromRows ys)
          (KMeansOpts KMeansPP (VU.fromListN 7 [1 .. 7]) False)
  liftIO $ writeKMeansModel filePath model
  return model
  

kmeansVecSink
  :: FilePath
  -> Int
  -> Int
  -> Sink [Vector Double] (ResourceT IO) (KMeans (Vector Double))
kmeansVecSink filePath k numTrain  = do
  xs <- CL.take numTrain
  let ys = L.concat xs
      model =
        kmeans
          k
          (MU.fromRows ys)
          (KMeansOpts KMeansPP (VU.fromListN 7 [1 .. 7]) False)
  liftIO $ writeKMeansModel filePath model
  return model

writeKMeansModel :: FilePath -> KMeans (Vector Double) -> IO ()
writeKMeansModel filePath = encodeFile filePath . MU.toLists . centers

readKMeansModel :: FilePath -> IO [[Double]]
readKMeansModel = decodeFile
