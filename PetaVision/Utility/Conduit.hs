module PetaVision.Utility.Conduit where

import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           PetaVision.Utility.Parallel

mapP
  :: NFData b
  => ParallelParams -> (a -> b) -> Conduit a (ResourceT IO) b
mapP parallelParams f = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys = parMapChunk parallelParams rdeepseq f xs
        sourceList ys
        mapP parallelParams f)
