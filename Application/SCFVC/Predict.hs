module Main where

import           Application.SCFVC.ArgsParser   as Parser
import           Application.SCFVC.FisherVector
import           Classifier.LibLinear
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Vector.Unboxed            as VU
import           Foreign.Ptr
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel    as Parallel
import           Prelude                        as P
import           System.Environment

main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  actHeader <- readPVPHeader . P.head $ actFile params
  errorHeader <- readPVPHeader . P.head $ errorFile params
  let parallelParams =
        ParallelParams (Parser.numThread params) (Parser.batchSize params)
  pvpFileSource (P.head $ errorFile params) =$=
    mergeSource (pvpFileSource (P.head $ actFile params)) $$
    fisherVectorConduit parallelParams (poolingFlag params) =$=
    CL.mapM (getFeatureVecPtr . Sparse . VU.toList) =$=
    mergeSource (labelSource $ labelFile params) =$=
    predict (modelName params) ((modelName params) P.++ ".out")
