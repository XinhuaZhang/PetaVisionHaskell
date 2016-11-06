module Application.GMM.Conduit where

import           Application.GMM.GMM
import           Application.GMM.Representation
import           Control.Monad.IO.Class
import           Data.Array.Repa                as R
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Vector                    as V
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel
import           Prelude                        as P

featureConduit
  :: ParallelParams -> Conduit PVPOutputData IO (V.Vector GMMData)
featureConduit parallelParams =
  do xs <- CL.take (batchSize parallelParams)
     if P.length xs > 0
        then do let ys =
                      parMapChunk parallelParams rdeepseq parsePVPOutputData xs
                sourceList ys
                featureConduit parallelParams
        else return ()
  where parsePVPOutputData
          :: PVPOutputData -> V.Vector GMMData
        parsePVPOutputData (PVP_OUTPUT_NONSPIKING_ACT (PVPDimension nx ny nf) xs) =
          let arr =
                fromListUnboxed (shapeOfList [nf,nx,ny] :: DIM3)
                                xs
          in V.fromList . P.map (fromListDense . R.toList . R.slice arr) $
             [(Z :. j :. i :. All)|j <- [0 .. (ny - 1)],i <- [0 .. (nx - 1)]]
        parsePVPOutputData (PVP_OUTPUT_ACT_SPARSEVALUES (PVPDimension nx ny nf) xs) =
          V.fromList .
          P.map (fromListSparse nf . P.map (\((_,j),x) -> (j,x))) .
          L.groupBy (\((ix,_),_) ((iy,_),_) -> ix == iy) .
          P.map (\(i,x) -> ((div i nf,mod i nf),x)) $
          xs
