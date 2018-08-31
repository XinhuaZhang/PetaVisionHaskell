import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Conduit                 as C
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel
import           System.Environment

{-# INLINE l2norm #-}

l2norm :: VU.Vector Double -> Double
l2norm = VU.sum . VU.map (^ (2 :: Int))

main = do
  (imgWeightFile:groundTruthWeightFile:_) <- getArgs
  ((PVP_OUTPUT_KERNEL imgWeight):_) <-
    runResourceT $ pvpFileSource imgWeightFile $$ CL.take 1
  ((PVP_OUTPUT_KERNEL groundTruthWeight):_) <-
    runResourceT $ pvpFileSource groundTruthWeightFile $$ CL.take 1
  let (Z :. _ :. _ :. _ :. numFeature1) = extent imgWeight
      (Z :. _ :. _ :. _ :. numFeature2) = extent groundTruthWeight
      !normList =
        parMap
          rdeepseq
          (\i ->
             let norm1 =
                   l2norm . toUnboxed . computeS . R.slice imgWeight $
                   (Z :. All :. All :. All :. i)
                 norm2 =
                   l2norm . toUnboxed . computeS . R.slice groundTruthWeight $
                   (Z :. All :. All :. All :. i)
             in (sqrt $ norm1 + norm2, sqrt norm1, sqrt norm2))
          [0 :: Int .. 100 - 1]
  if numFeature1 == numFeature2
    then M.mapM_ print normList
    else error $
         "numFeatures are not equal: " L.++ show numFeature1 L.++ " " L.++
         show numFeature2
