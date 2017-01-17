module Main where

import           Application.GMM.ArgsParser   as Parser
import           Application.GMM.Conduit
import           Application.GMM.GMM
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.Binary          as CB
import           Data.Conduit.List            as CL
import           PetaVision.Data.Pooling
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Array
import           PetaVision.Utility.Conduit
import           PetaVision.Utility.Parallel
import           Prelude                      as P
import           System.Environment
import           System.IO

main = do
  args <- getArgs
  if null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  let parallelParams =
        ParallelParams (Parser.numThread params) (Parser.batchSize params)
  print params
  runResourceT
    (pvpFileSource (P.head $ pvpFile params) $$
     poolArrayConduit
       (ParallelParams (Parser.numThread params) (Parser.batchSize params))
       (poolingType params)
       (poolingSize params)
       undefined =$=
     mapP parallelParams extractFeaturePoint =$=
     gmmSink
       parallelParams
       (gmmFile params)
       (numGaussian params)
       (threshold params)
       100)
