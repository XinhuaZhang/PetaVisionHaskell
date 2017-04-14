{-# LANGUAGE FlexibleContexts #-}
module Application.Image2PVPFile.Conduit where

import           Control.Monad                 as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa               as R
import           Data.Conduit                  as C
import           Data.Conduit.List             as CL
import           Data.List                     as L
import           PetaVision.Data.Image.ImageIO
import           PetaVision.Data.Image.Utility
import           PetaVision.PVPFile.Types
import           PetaVision.Utility.Parallel

rescaleSquareImageConduit
  :: (R.Source s Double)
  => ParallelParams
  -> (Int, Int)
  -> (Double, Double)
  -> Conduit (Array s DIM3 Double) (ResourceT IO) PVPOutputData
rescaleSquareImageConduit parallelParams newSize bound = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (image2PVPData . rescaleImage25D newSize bound . squareImage)
                xs
        sourceList ys
        rescaleSquareImageConduit parallelParams newSize bound)
