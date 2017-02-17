{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}

module PetaVision.Data.KMeans
  ( ClusterCenter
  , kmeans
  , computeSoftAssignment
  , writeKMeansCenter
  , readKMeansCenter
  ) where

import           Control.Arrow               ((&&&))
import           Data.Binary
import           Data.List                   as L
import           Data.Vector                 as V
import           Data.Vector.Unboxed         as VU
import           PetaVision.Utility.Parallel
import           System.Random

type ClusterCenter = V.Vector (VU.Vector Double)

type Assignment = V.Vector Int

randomClusterCenter :: Int -> [VU.Vector Double] -> IO ClusterCenter
randomClusterCenter k =
  V.replicateM k .
  VU.mapM randomRIO .
  VU.fromList . L.map (L.minimum &&& L.maximum) . L.transpose . L.map VU.toList

computeAssignmentP :: ClusterCenter
                   -> [V.Vector (VU.Vector Double)]
                   -> [Assignment]
computeAssignmentP = parMap rdeepseq . computeAssignment

computeMeanP
  :: Int
  -> Int
  -> [Assignment]
  -> [V.Vector (VU.Vector Double)]
  -> [V.Vector (VU.Vector Double)]
computeMeanP k nf = parZipWith rdeepseq (computeMean k nf)

computeDistortionP :: ClusterCenter
                   -> [Assignment]
                   -> [V.Vector (VU.Vector Double)]
                   -> Double
computeDistortionP clusterCenter assignments =
  L.sum . parZipWith rdeepseq (computeDistortion clusterCenter) assignments

meanList2ClusterCenter :: [V.Vector (VU.Vector Double)] -> ClusterCenter
meanList2ClusterCenter vecs =
  V.map (VU.map (/ (fromIntegral . L.length $ vecs))) .
  L.foldl1' (V.zipWith (VU.zipWith (+))) $
  vecs

kmeans :: ParallelParams -> Int -> Double -> [VU.Vector Double] -> IO ClusterCenter
kmeans parallelParams k threshold xs = do
  randomCenter <- randomClusterCenter k xs
  go randomCenter .
    L.map V.fromList . splitList (div (L.length xs) (numThread parallelParams)) $
    xs
  where
    nf = VU.length . L.head $ xs
    go !center ys = do
      let assignment = computeAssignmentP center ys
          newCenter = meanList2ClusterCenter . computeMeanP k nf assignment $ ys
          distortion = computeDistortionP center assignment ys
      print distortion
      if distortion < threshold
        then return newCenter
        else go newCenter ys

computeSoftAssignment :: ParallelParams
                      -> ClusterCenter
                      -> [VU.Vector Double]
                      -> [VU.Vector Double]
computeSoftAssignment parallelParams center =
  parMapChunk
    parallelParams
    rdeepseq
    (\x ->
        let dist = V.map (distFunc x) center
            mean = V.sum dist / fromIntegral (V.length center)
        in V.convert . V.map (\y -> max 0 (mean - y)) $ dist)


writeKMeansCenter :: FilePath -> ClusterCenter -> IO ()
writeKMeansCenter filePath = encodeFile filePath . V.toList . V.map VU.toList

readKMeansCenter :: FilePath -> IO ClusterCenter
readKMeansCenter filePath = do
  xs <- decodeFile filePath
  return . V.fromList . L.map VU.fromList $ xs

{-# INLINE computeAssignment #-}

computeAssignment :: ClusterCenter -> V.Vector (VU.Vector Double) -> Assignment
computeAssignment cluster =
  V.map (\x -> V.minIndex . V.map (distFunc x) $ cluster)

{-# INLINE computeMean #-}

computeMean
  :: Int
  -> Int
  -> Assignment
  -> V.Vector (VU.Vector Double)
  -> V.Vector (VU.Vector Double)
computeMean k nf assignmet =
  V.map (\(s, count) -> VU.map (/ count) s) .
  V.accumulate
    (\(s, count) vec -> (VU.zipWith (+) s vec, count + 1))
    (V.replicate k (VU.replicate nf 0, 0)) .
  V.zip assignmet

{-# INLINE computeDistortion #-}

computeDistortion :: ClusterCenter
                  -> Assignment
                  -> V.Vector (VU.Vector Double)
                  -> Double
computeDistortion clusterCenter assignments =
  V.sum .
  V.zipWith (\assignment vec -> distFunc vec (clusterCenter V.! assignment)) assignments

{-# INLINE distFunc #-}

distFunc :: VU.Vector Double -> VU.Vector Double -> Double
distFunc vec1 vec2 = VU.sum $ VU.zipWith (\a b -> (a - b) ^ (2 :: Int)) vec1 vec2

{-# INLINE splitList #-}

splitList :: Int -> [a] -> [[a]]
splitList _ [] = []
splitList n ys = as : splitList n bs
  where
    (as, bs) = L.splitAt n ys
