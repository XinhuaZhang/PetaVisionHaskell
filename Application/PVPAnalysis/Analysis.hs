module Application.PVPAnalysis.Analysis where

import           Control.Arrow
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           PetaVision.PVPFile.IO
import           Prelude                      as P
import           Text.Printf


sparsity :: PVPHeader -> Sink PVPOutputData (ResourceT IO) ()
sparsity header =
  do numActive <-
       CL.fold (\b a ->
                  case a of
                    PVP_OUTPUT_ACT _ _ ->
                      error "Cannot compute PVP_OUTPUT_ACT file's sparsity"
                    PVP_OUTPUT_NONSPIKING_ACT _ x ->
                      b + (VU.length . VU.filter (/= 0) $ x)
                    PVP_OUTPUT_ACT_SPARSEVALUES _ x -> b + (VU.length x)
                    _ ->
                      error $
                      "computeActivationHistogram: " L.++ show a L.++
                      " is not supported.")
               0
     let percent = fromIntegral numActive / fromIntegral totalNumEle
     liftIO $
       printf "%0.1f%%(%d/%d Avg. Activatived)\n"
              (percent * 100 :: Double)
              (round $ percent * fromIntegral (nf header) :: Int)
              (nf header)
  where totalNumEle = (nBands header) * (nx header) * (ny header) * (nf header)

sparsityActivationHistogramSink
  :: PVPHeader -> Sink PVPOutputData (ResourceT IO) (Double,VU.Vector Double)
sparsityActivationHistogramSink header =
  do (numActive,actCount) <-
       CL.fold (\(b,vec) a ->
                  let newB =
                        case a of
                          PVP_OUTPUT_ACT _ _ ->
                            error "Cannot compute PVP_OUTPUT_ACT file's sparsity"
                          PVP_OUTPUT_NONSPIKING_ACT _ x ->
                            b + (VU.length . VU.filter (/= 0) $ x)
                          PVP_OUTPUT_ACT_SPARSEVALUES _ x -> b + (VU.length x)
                          _ ->
                            error $
                            "computeActivationHistogram: " L.++ show a L.++
                            " is not supported."
                      newVec = computeActivationHistogram vec a
                  in (newB,newVec))
               (0
               ,VU.replicate ((nx header) * (ny header) * (nf header))
                             0)
     let totalNumEle =
           (nBands header) * (nx header) * (ny header) * (nf header)
         percent = fromIntegral numActive / fromIntegral totalNumEle
         actCountVec =
           toUnboxed .
           computeS .
           R.map (\x -> (fromIntegral x :: Double) / (fromIntegral ((nBands header) * (nx header) * (ny header)))) .
           sumS .
           sumS .
           R.backpermute (Z :. nf header :. ny header :. nx header)
                         (\(Z :. k :. j :. i) -> (Z :. j :. i :. k)) .
           fromUnboxed (Z :. ny header :. nx header :. nf header) $
           actCount
     liftIO $
       printf "Sparsity: %0.1f%%(%d/%d Avg. Activatived)\n"
              (percent * 100 :: Double)
              (round $ percent * fromIntegral (nf header) :: Int)
              (nf header)
     return (percent,actCountVec)

{-# INLINE computeActivationHistogram #-}

computeActivationHistogram
  :: VU.Vector Int -> PVPOutputData -> VU.Vector Int
computeActivationHistogram defaultVec (PVP_OUTPUT_NONSPIKING_ACT pvpDimension x) =
  VU.zipWith (+) defaultVec .
  VU.map (\y ->
            if y /= 0
               then 1
               else 0) $
  x
computeActivationHistogram defaultVec (PVP_OUTPUT_ACT_SPARSEVALUES _ x) =
  VU.accumulate (+) defaultVec . VU.map (second $ const 1) $ x
computeActivationHistogram _ x =
  error $ "computeActivationHistogram: " L.++ show x L.++ " is not supported."


averageError :: PVPHeader -> Sink PVPOutputData (ResourceT IO) ()
averageError header = do
  errorSum <-
    CL.fold
      (\b a ->
          case a of
            PVP_OUTPUT_NONSPIKING_ACT _ x ->
              b + sqrt (VU.sum . VU.map (^ (2 :: Int)) $ x)
            _ -> error "sparsity: pvpFile type is not supported.")
      0
  let numEle = nx header * ny header * nf header
      avgError = errorSum / fromIntegral numEle / fromIntegral (nBands header)
  liftIO . print $ avgError
