{-# LANGUAGE BangPatterns #-}

module Application.GMM.FisherKernel where

import           Application.GMM.Gaussian
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Control.Monad
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           PetaVision.Utility.Parallel

fisherVectorMu
  :: ParallelParams
  -> [GMM]
  -> [Assignment]
  -> [(Int, VU.Vector Double)]
  -> VU.Vector Double
fisherVectorMu parallelParams gmms assignments xs =
  VU.concat $
  parZipWith3Chunk
    parallelParams
    rdeepseq
    (\(MixtureModel _ modelVec) (assignment0, assignment) (nz, x) ->
        let !muVec = V.convert . V.map (\(Model m) -> gaussianMu . snd $ m) $ modelVec
            !sigmaVec =
              V.convert . V.map (\(Model m) -> gaussianSigma . snd $ m) $ modelVec
            !wVec = V.convert . V.map (\(Model m) -> fst m) $ modelVec
        in VU.zipWith3
             (\wk sigma' y ->
                 y * sqrt (sigma' / (fromIntegral (VU.length x + nz) * wk)))
             wVec
             sigmaVec .
           VU.zipWith
             (+)
             (VU.zipWith3
                (\mu' sigma' a -> fromIntegral nz * a * (-mu') / sigma')
                muVec
                sigmaVec
                assignment0) .
           V.foldl1' (VU.zipWith (+)) .
           V.zipWith
             (\an xn ->
                 VU.zipWith3
                   (\mu' sigma' a -> a * (xn - mu') / sigma')
                   muVec
                   sigmaVec
                   an)
             assignment .
           VU.convert $
           x)
    gmms
    assignments
    xs

fisherVectorSigma
  :: ParallelParams
  -> [GMM]
  -> [Assignment]
  -> [(Int, VU.Vector Double)]
  -> VU.Vector Double
fisherVectorSigma parallelParams gmms assignments xs =
  VU.concat $
  parZipWith3Chunk
    parallelParams
    rdeepseq
    (\(MixtureModel _ modelVec) (assignment0, assignment) (nz, x) ->
        let !muVec = V.convert . V.map (\(Model m) -> gaussianMu . snd $ m) $ modelVec
            !sigmaVec =
              V.convert . V.map (\(Model m) -> gaussianSigma . snd $ m) $ modelVec
            !wVec = V.convert . V.map (\(Model m) -> fst m) $ modelVec
        in VU.zipWith
             (\wk y -> y * sqrt (0.5 / (fromIntegral (VU.length x + nz) * wk)))
             wVec .
           VU.zipWith
             (+)
             (VU.zipWith3
                (\mu' sigma' an ->
                    fromIntegral nz * an * (mu' ^ (2 :: Int) / sigma' - 1))
                muVec
                sigmaVec
                assignment0) .
           V.foldl1' (VU.zipWith (+)) .
           V.zipWith
             (\an xn ->
                 VU.zipWith3
                   (\mu' sigma' a -> a * ((xn - mu') ^ (2 :: Int) / sigma' - 1))
                   muVec
                   sigmaVec
                   an)
             assignment .
           VU.convert $
           x)
    gmms
    assignments
    xs

fisherVectorConduit
  :: ParallelParams
  -> [GMM]
  -> Conduit (Int, [(Int, VU.Vector Double)]) IO (Int, VU.Vector Double)
fisherVectorConduit parallelParams gmms =
  awaitForever
    (\(label, x) ->
        let !assignments =
              parZipWithChunk parallelParams rdeepseq getAssignmentVec gmms .
              snd . L.unzip $
              x
            !vecMu = fisherVectorMu parallelParams gmms assignments x
            !vecSigma = fisherVectorSigma parallelParams gmms assignments x
            !vec = vecMu VU.++ vecSigma
            !l2Norm = sqrt (VU.foldl' (\a b -> a + b ^ (2 :: Int)) 0 vec)
        in yield (label, VU.map (/ l2Norm) vec))
