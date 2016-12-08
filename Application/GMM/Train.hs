module Main where

import           Application.GMM.ArgsParser           as Parser
import           Application.GMM.ConvertPVPGMMConduit
import           Application.GMM.GMM
import           Data.Array.Repa                      as R
import           Data.Conduit
import           Data.Conduit.Binary                  as CB
import           Data.Conduit.List                    as CL
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel
import           Prelude                              as P
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
  sourceFile (P.head $ pvpFile params) $$ featureConduit =$=
    gmmSink
      parallelParams
      (gmmFile params)
      (numGaussian params)
      192
      ((0, 500), (0.1, 1000))
      (threshold params)
