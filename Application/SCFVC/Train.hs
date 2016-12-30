module Main where

import           Application.SCFVC.ArgsParser   as Parser
import           Application.SCFVC.FisherVector
import           Classifier.LibLinear
import           Control.Monad.IO.Class
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Vector.Unboxed            as VU
import           Foreign.Ptr
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel    as Parallel
import           Prelude                        as P
import           System.Environment


featureConduit :: Conduit (VU.Vector Double) IO (Ptr C'feature_node)
featureConduit =
  awaitForever
    (\x ->
       do ptr <- liftIO . getFeatureVecPtr . Sparse . VU.toList $ x
          yield ptr)

trainSink
  :: ParallelParams -> FilePath -> TrainParams -> Bool -> Sink (Ptr C'feature_node) IO ()
trainSink parallelParams filePath trainParams findCFlag = do
  featurePtrs <- consume
  -- if ((VU.length . P.head $ xs) /= (trainFeatureIndexMax trainParams))
  --   then error $
  --        "Number of feature in trainParams is not correct. (" P.++
  --        (show . VU.length . P.head $ xs) P.++
  --        " vs " P.++
  --        (show $ trainFeatureIndexMax trainParams) P.++
  --        ")"
  --   else return ()
  -- featurePtrs <- liftIO $ P.mapM (getFeatureVecPtr . Sparse . VU.toList) xs
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
  print params
  actHeader <- readPVPHeader . P.head $ actFile params
  errorHeader <- readPVPHeader . P.head $ errorFile params
  let parallelParams =
        ParallelParams (Parser.numThread params) (Parser.batchSize params)
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = c params
        , trainNumExamples = nBands actHeader
        , trainFeatureIndexMax =
          nf errorHeader * nf actHeader *
          if poolingFlag params
            then 1
            else ny errorHeader * nx errorHeader
        , trainModel = modelName params
        }
  pvpFileSource (P.head $ errorFile params) =$=
    mergeSource (pvpFileSource (P.head $ actFile params)) $$
    fisherVectorConduit parallelParams (poolingFlag params) =$=
    featureConduit =$=
    trainSink parallelParams (labelFile params) trainParams (findC params)
