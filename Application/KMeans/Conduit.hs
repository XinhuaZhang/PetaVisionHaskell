{-# LANGUAGE FlexibleContexts #-}

module Application.KMeans.Conduit where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           PetaVision.Data.KMeans       as KMP
import           PetaVision.Utility.Array
import           PetaVision.Utility.Parallel

kmeansArrSinkP
  :: (R.Source s Double)
  => ParallelParams
  -> FilePath
  -> Int
  -> Int
  -> Int
  -> Int
  -> Sink (Array s DIM3 Double) (ResourceT IO) ()
kmeansArrSinkP parallelParams filePath k numTrain downsampleFactor stride =
  do xs <- CL.take numTrain
     let ys =
           L.concatMap
             (extractFeaturePoint .
              downsample [downsampleFactor,downsampleFactor,1])
             xs
         (Z :. _r :. _c :. ch) = extent . L.head $ xs
     model <-
       liftIO $
       KMP.kmeans parallelParams
                  k
                  (KMP.Shape 1 1 ch stride)
                  ys
     liftIO $ encodeFile filePath model

kmeansVecSinkP
  :: ParallelParams
  -> FilePath
  -> Int
  -> Int
  -> KMP.Shape
  -> Sink [Vector Double] (ResourceT IO) ()
kmeansVecSinkP parallelParams filePath k numTrain sh =
  do xs <- CL.take numTrain
     let ys = L.concat xs
     model <- liftIO $ kmeans parallelParams k sh ys
     liftIO $ encodeFile filePath model
