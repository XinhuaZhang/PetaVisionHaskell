{-# LANGUAGE BangPatterns #-}
module Application.GMM.FisherKernel where

import           Application.GMM.Gaussian
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Control.Monad
import           Control.Monad.Trans.Resource
import           PetaVision.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.List                            as CL
import           Data.List                                    as L
import           Data.Vector.Unboxed                          as VU

fisherVectorMu :: GMM -> [VU.Vector Double] -> VU.Vector Double
fisherVectorMu gmm xs =
  VU.map (/ (sqrt . fromIntegral . L.length $ xs)) . L.foldl1' (VU.zipWith (+)) $
  L.zipWith
    (\assignment x ->
        VU.concat .
        L.zipWith
          (\a (Model (w, Gaussian m s)) ->
              VU.zipWith3 (\xi mi si -> a * (xi - mi) / sqrt si / sqrt w) x m s)
          assignment .
        model $
        gmm)
    assignments
    xs
  where
    assignments = getAssignmentVec gmm xs

fisherVectorSigma :: GMM
                  -> [VU.Vector Double]
                  -> VU.Vector Double
fisherVectorSigma gmm xs =
  VU.map (/ (sqrt . (*) 2 . fromIntegral . L.length $ xs)) .
  L.foldl1' (VU.zipWith (+)) $
  L.zipWith
    (\assignment x ->
        VU.concat .
        L.zipWith
          (\a (Model (w, Gaussian m s)) ->
              VU.zipWith3
                (\xi mi si -> a * ((xi - mi) ^ (2 :: Int) / si - 1) / sqrt w)
                x
                m
                s)
          assignment .
        model $
        gmm)
    assignments
    xs
  where
    assignments = getAssignmentVec gmm xs
    
fisherVector :: GMM -> [VU.Vector Double] -> VU.Vector Double
fisherVector gmm x
  | l2Norm == 0 = VU.replicate (VU.length vec) 0
  | otherwise = VU.map (/ l2Norm) powerVec
  where
    !vecMu = fisherVectorMu gmm x
    !vecSigma = fisherVectorSigma gmm x
    !vec = vecMu VU.++ vecSigma
    !powerVec = VU.map (\x' -> signum x' * (abs x' ** 0.5)) vec
    !l2Norm = sqrt (VU.foldl' (\a b -> a + b ^ (2 :: Int)) 0 powerVec)

fisherVectorConduit
  :: ParallelParams
  -> GMM
  -> Conduit [VU.Vector Double] (ResourceT IO) (VU.Vector Double)
fisherVectorConduit parallelParams gmm = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\x ->
                    let !vecMu = fisherVectorMu gmm x
                        !vecSigma = fisherVectorSigma gmm x
                        !vec = vecMu VU.++ vecSigma
                        !powerVec =
                          VU.map (\x' -> signum x' * (abs x' ** 0.5)) vec
                        !l2Norm =
                          sqrt
                            (VU.foldl' (\a b -> a + b ^ (2 :: Int)) 0 powerVec)
                        !result =
                          if l2Norm == 0
                            then VU.replicate (VU.length vec) 0
                            else VU.map (/ l2Norm) powerVec
                    in result)
                xs
        sourceList ys
        fisherVectorConduit parallelParams gmm)




fisherVectorConduit1
  :: ParallelParams
  -> [GMM]
  -> Conduit (Int, [[VU.Vector Double]]) (ResourceT IO) (Int, VU.Vector Double)
fisherVectorConduit1 parallelParams gmms = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(label, zs) ->
                    let !vecMus = L.zipWith fisherVectorMu gmms zs
                        !vecSigmas = L.zipWith fisherVectorSigma gmms zs
                        !vecs = L.zipWith (VU.++) vecMus vecSigmas
                        !powerVecs =
                          L.map (VU.map (\x' -> signum x' * (abs x' ** 0.5))) vecs
                        !l2Norms =
                          L.map
                            (sqrt . VU.foldl' (\a b -> a + b ^ (2 :: Int)) 0)
                            powerVecs
                        !result =
                          VU.concat $
                          L.zipWith
                            (\l2Norm powerVec ->
                                if l2Norm == 0
                                  then VU.replicate (VU.length powerVec) 0
                                  else VU.map (/ l2Norm) powerVec)
                            l2Norms
                            powerVecs
                    in (label, result))
                xs
        sourceList ys
        fisherVectorConduit1 parallelParams gmms)
