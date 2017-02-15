module Main where

import           Application.GMM.ArgsParser   as Parser
import           Application.GMM.Conduit
import           Application.GMM.FisherKernel
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Classifier.LibLinear
import           Control.Monad                as M
import           Control.Monad.Parallel                as MP
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Time
import           Data.Vector.Unboxed          as VU
import           Foreign.Ptr
import           PetaVision.Data.Pooling
import           PetaVision.Data.Pooling
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Array
import           PetaVision.Utility.Conduit
import           PetaVision.Utility.Parallel  as Parallel
import           Prelude                      as P
import           System.Environment

featurePointConduit :: ParallelParams -> Conduit (VU.Vector Double) (ResourceT IO) (Ptr C'feature_node)
featurePointConduit parallelParams = do xs <- CL.take (Parallel.batchSize parallelParams)
                                        unless (L.null xs) (do ptrs <- liftIO . MP.mapM (getFeatureVecPtr . Sparse. VU.toList) $ xs
                                                               sourceList ptrs
                                                               featurePointConduit parallelParams)

trainSink
  :: ParallelParams -> FilePath -> TrainParams -> Bool -> Sink (Ptr C'feature_node) (ResourceT IO) ()
trainSink parallelParams filePath trainParams findCFlag =
  do featurePtrs <- consume
     -- if ((VU.length . P.head $ xs) /= (trainFeatureIndexMax trainParams))
     --    then error $
     --         "Number of feature in trainParams is not correct. (" P.++
     --         (show . VU.length . P.head $ xs) P.++
     --         " vs " P.++
     --         (show $ trainFeatureIndexMax trainParams) P.++
     --         ")"
     --    else return ()
     -- featurePtrs <-
     --   liftIO $ M.mapM (getFeatureVecPtr . Dense . VU.toList) xs
     label <- liftIO $ readLabelFile filePath
     if findCFlag
        then liftIO $ findParameterC trainParams label featurePtrs
        else liftIO $ train trainParams label featurePtrs
     -- go label []
  where go :: [Double]
           -> [[Ptr C'feature_node]]
           -> Sink (Ptr C'feature_node) (ResourceT IO) ()
        go label pss =
          do xs <- CL.take (Parallel.batchSize parallelParams)
             if P.length xs > 0
                then do 
                        time <- liftIO getZonedTime
                        liftIO $
                          print . localTimeOfDay . zonedTimeToLocalTime $ time
                        go label $! (xs : pss)
                else liftIO $
                     train trainParams label (P.concat . L.reverse $ pss)

main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  header <- readPVPHeader . P.head $ pvpFile params
  gmm <- readGMM (gmmFile params) :: IO [GMM]
  muVarList <- decodeFile (muVarFile params)
  let parallelParams =
        ParallelParams (Parser.numThread params) (Parser.batchSize params)
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = c params
        , trainNumExamples = nBands header
        , trainFeatureIndexMax =
            2 * (nf header) * (numModel . P.head $ gmm) *
            (gridNum
               (poolingSize params)
               (div ((nx header) - (poolingSize params) + 1) (poolingStride params)) --(poolingStride params)
               ((nx header) - (poolingSize params) + 1)
               ((ny header) - (poolingSize params) + 1))
        , trainModel = modelName params
        }
      muVar = VU.fromList muVarList
  print params
  print trainParams
  runResourceT $
    pvpFileSource (P.head $ pvpFile params) $$
    poolArrayConduit
      (ParallelParams (Parser.numThread params) (Parser.batchSize params))
      (poolingType params)
      (poolingSize params)
      undefined =$= mapP parallelParams (poolGridList 3 1 (toUnboxed . R.computeS)) =$=
    mapP
      parallelParams
      (poolGrid
         (div ((nx header) - (poolingSize params) + 1) (poolingStride params))
         (div ((nx header) - (poolingSize params) + 1) (poolingStride params))
         (fisherVector (P.head gmm) muVar . extractFeaturePoint)) =$= featurePointConduit parallelParams =$=
    trainSink parallelParams (labelFile params) trainParams (findC params)
