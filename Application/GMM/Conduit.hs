{-# LANGUAGE BangPatterns #-}
module Application.GMM.Conduit where

import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel
import           Prelude                      as P

featureConduit
  :: ParallelParams -> Conduit PVPOutputData (ResourceT IO) [VU.Vector Double]
featureConduit parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys = parMapChunk parallelParams rdeepseq parsePVPOutputData xs
        sourceList ys
        featureConduit parallelParams)
  where
    parsePVPOutputData :: PVPOutputData -> [VU.Vector Double]
    parsePVPOutputData (PVP_OUTPUT_NONSPIKING_ACT (PVPDimension nx' ny' nf') xs) =
      let !arr = fromUnboxed (shapeOfList [nf', nx', ny'] :: DIM3) xs
      in P.map
           (VU.fromList . R.toList . R.slice arr)
           [ Z :. j :. i :. All
           | j <- [0 .. (ny' - 1)]
           , i <- [0 .. (nx' - 1)] ]
    parsePVPOutputData (PVP_OUTPUT_ACT_SPARSEVALUES (PVPDimension nx' ny' nf') xs) =
      let !vec = VU.accumulate (+) (VU.replicate (nx'*ny'*nf') 0) xs
          !arr = fromUnboxed (shapeOfList [nf', nx', ny'] :: DIM3) vec
      in P.map
           (VU.fromList . R.toList . R.slice arr)
           [ Z :. j :. i :. All
           | j <- [0 .. (ny' - 1)]
           , i <- [0 .. (nx' - 1)] ]
    parsePVPOutputData _ = error "featureConduit: output format is not supported."
