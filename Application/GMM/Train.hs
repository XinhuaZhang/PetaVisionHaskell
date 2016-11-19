module Main where

import           Application.GMM.ArgsParser  as Parser
import           Application.GMM.Conduit
import           Application.GMM.GMM
import           Data.Array.Repa             as R
import           Data.Conduit
import           Data.Conduit.List           as CL
import           PetaVision.Data.Pooling
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel
import           Prelude                     as P
import           System.Environment

main =
  do args <- getArgs
     if null args
        then error "run with --help to see options."
        else return ()
     params <- parseArgs args
     let parallelParams =
           ParallelParams (Parser.numThread params)
                          (Parser.batchSize params)
     print params
     pvpFileSource (P.head $ pvpFile params) $$
       poolConduit parallelParams
                   (poolingType params)
                   (poolingSize params)
                   0 =$=
      pooledFeatureConduit parallelParams 192 =$=
       -- featureConduit parallelParams =$=
       gmmSink parallelParams
               (numGaussian params)
               (threshold params)
               (gmmFile params)
