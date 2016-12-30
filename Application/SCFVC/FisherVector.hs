{-# LANGUAGE BangPatterns #-}

module Application.SCFVC.FisherVector where

import           Control.Arrow
import           Control.Monad               as M
import           Data.Array.Repa             as R
import           Data.Conduit
import           Data.Conduit.List           as CL
import           Data.List                   as L
import           Data.Vector.Unboxed         as VU
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel

fisherVectorConduit :: ParallelParams -> Bool
                    -> Conduit (PVPOutputData, PVPOutputData) IO (VU.Vector Double)
fisherVectorConduit parallelParams poolFlag = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let func1 = uncurry fisherVector . join (***) pvpOutputData2Array
            func2 =
              if poolFlag
                then toUnboxed . foldS (+) 0 . foldS (+) 0 . func1
                else toUnboxed . computeS . func1
            !ys = parMapChunk parallelParams rdeepseq func2 xs
        sourceList ys
        fisherVectorConduit parallelParams poolFlag)

fisherVector :: Array U DIM3 Double -> Array U DIM3 Double -> Array D DIM3 Double
fisherVector act err =
  traverse2
    act
    err
    (\(Z :. _nyAct :. _nxAct :. nfAct) (Z :. nyErr :. nxErr :. nfErr) ->
        (Z :. nfErr * nfAct :. nyErr :. nxErr)) $
  \fAct fErr (Z :. k :. j :. i) ->
     let !kAct = k `div` nfErr'
         !kErr = k `mod` nfErr'
     in fAct (Z :. j :. i :. kAct) * fErr (Z :. j :. i :. kErr)
  where
    !(Z :. _nyErr' :. _nxErr' :. nfErr') = extent err
