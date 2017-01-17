module Main where

import           Application.GMM.ArgsParser   as Parser
import           Application.GMM.Conduit
import           Application.GMM.FisherKernel
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Classifier.LibLinear
import           Control.Monad                as M
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
import           PetaVision.Utility.Parallel  as Parallel
import           Prelude                      as P
import           System.Environment

trainSink
  :: ParallelParams -> FilePath -> TrainParams -> Bool -> Sink (VU.Vector Double) (ResourceT IO) ()
trainSink parallelParams filePath trainParams findCFlag =
  do xs <- consume
     if ((VU.length . P.head $ xs) /= (trainFeatureIndexMax trainParams))
        then error $
             "Number of feature in trainParams is not correct. (" P.++
             (show . VU.length . P.head $ xs) P.++
             " vs " P.++
             (show $ trainFeatureIndexMax trainParams) P.++
             ")"
        else return ()
     featurePtrs <-
       liftIO $ M.mapM (getFeatureVecPtr . Dense . VU.toList) xs
     label <- liftIO $ readLabelFile filePath
     if findCFlag
        then liftIO $ findParameterC trainParams label featurePtrs
        else liftIO $ train trainParams label featurePtrs -- go label []
  where go :: [Double]
           -> [[Ptr C'feature_node]]
           -> Sink (VU.Vector Double) IO ()
        go label pss =
          do xs <- CL.take (Parallel.batchSize parallelParams)
             if P.length xs > 0
                then do ps <-
                          liftIO $
                          P.mapM (getFeatureVecPtr . Dense . VU.toList) xs
                        time <- liftIO getZonedTime
                        liftIO $
                          print . localTimeOfDay . zonedTimeToLocalTime $ time
                        go label $! (ps : pss)
                else liftIO $
                     train trainParams label (P.concat . L.reverse $ pss)

main =
  do args <- getArgs
     if P.null args
        then error "run with --help to see options."
        else return ()
     params <- parseArgs args
     header <- readPVPHeader . P.head $ pvpFile params
     gmm <- readGMM (gmmFile params) :: IO [GMM]
     let parallelParams =
           ParallelParams (Parser.numThread params)
                          (Parser.batchSize params)
         trainParams =
           TrainParams {trainSolver = L2R_L2LOSS_SVC_DUAL
                       ,trainC = c params
                       ,trainNumExamples = nBands header
                       ,trainFeatureIndexMax =
                          2 * (nf header) * (numModel . P.head $ gmm)
                       ,trainModel = modelName params}
     print params
     runResourceT $
       pvpFileSource (P.head $ pvpFile params) $$
       poolVecFeaturePointConduit
         (ParallelParams (Parser.numThread params)
                         (Parser.batchSize params))
         (poolingType params)
         (poolingSize params)
         undefined =$=
       fisherVectorConduit parallelParams
                           (P.head gmm) =$=
       trainSink parallelParams
                 (labelFile params)
                 trainParams
                 (findC params)
