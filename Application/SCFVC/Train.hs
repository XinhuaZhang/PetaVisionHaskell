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

trainSink
  :: ParallelParams -> FilePath -> TrainParams -> Bool -> Sink (VU.Vector Double) IO ()
trainSink parallelParams filePath trainParams findCFlag = do
  xs <- consume
  if ((VU.length . P.head $ xs) /= (trainFeatureIndexMax trainParams))
    then error $
         "Number of feature in trainParams is not correct. (" P.++
         (show . VU.length . P.head $ xs) P.++
         " vs " P.++
         (show $ trainFeatureIndexMax trainParams) P.++
         ")"
    else return ()
  featurePtrs <- liftIO $ P.mapM (getFeatureVecPtr . Sparse . VU.toList) xs
  label <- liftIO $ readLabelFile filePath
  if findCFlag
    then liftIO $ findParameterC trainParams label featurePtrs
    else liftIO $ train trainParams label featurePtrs

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
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = c params
        , trainNumExamples = nBands header
        , trainFeatureIndexMax =
          nf errorHeader * nf actHeader *
          if poolingFlag params
            then ny errorHeader * nx errorHeader
            else 1
        , trainModel = modelName params
        }
  pvpFileSource (P.head $ errorFile params) =$=
    mergeSource (pvpFileSource (P.head $ actFile params)) $$
    fisherVectorConduit parallelParams (poolingFlag params) =$=
    trainSink parallelParams (labelFile params) trainParams (findC params)
