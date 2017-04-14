module Application.PVPAnalysis.Analysis where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           PetaVision.PVPFile.IO
import           Prelude                      as P
import           Text.Printf


sparsity :: PVPHeader -> Sink PVPOutputData (ResourceT IO) ()
sparsity header = do
  numActive <-
    CL.fold
      (\b a ->
          case a of
            PVP_OUTPUT_ACT _ _ ->
              error "Cannot compute PVP_OUTPUT_ACT file's sparsity"
            PVP_OUTPUT_NONSPIKING_ACT _ x ->
              b + (VU.length . VU.filter (/= 0) $ x)
            PVP_OUTPUT_ACT_SPARSEVALUES _ x -> b + VU.length x
            _ -> error "sparsity: pvpFile type is not supported.")
      0
  let percent = fromIntegral numActive / fromIntegral totalNumEle
  liftIO $
    printf
      "%0.1f%%(%d/%d Avg. Activatived)\n"
      (percent * 100 :: Double)
      (round $ percent * fromIntegral (nf header) :: Int)
      (nf header)
  where
    totalNumEle = nBands header * nx header * ny header * nf header


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
