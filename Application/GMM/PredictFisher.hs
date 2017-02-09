module Main where

import           Application.GMM.ArgsParser   as Parser
import           Application.GMM.Conduit
import           Application.GMM.FisherKernel
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Classifier.LibLinear
import           Classifier.LibLinear.Example
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
import           PetaVision.Utility.Array
import           PetaVision.Utility.Conduit
import           PetaVision.Utility.Parallel  as Parallel
import           Prelude                      as P
import           System.Environment
import           Control.Monad.Parallel                as MP

featurePointConduit :: ParallelParams -> Conduit (VU.Vector Double) (ResourceT IO) (Ptr C'feature_node)
featurePointConduit parallelParams = do xs <- CL.take (Parallel.batchSize parallelParams)
                                        unless (L.null xs) (do ptrs <- liftIO . MP.mapM (getFeatureVecPtr . Sparse. VU.toList) $ xs
                                                               sourceList ptrs
                                                               featurePointConduit parallelParams)

main =
  do args <- getArgs
     if P.null args
        then error "run with --help to see options."
        else return ()
     params <- parseArgs args
     header <- readPVPHeader . P.head $ pvpFile params
     gmm <- readGMM (gmmFile params) :: IO [GMM]
     muVarList <- decodeFile (muVarFile params)
     let parallelParams =
           ParallelParams (Parser.numThread params)
                          (Parser.batchSize params)
         muVar = VU.fromList muVarList
     print params
     runResourceT $
       pvpFileSource (P.head $ pvpFile params) $$
       poolArrayConduit
         (ParallelParams (Parser.numThread params)
                         (Parser.batchSize params))
         (poolingType params)
         (poolingSize params)
         undefined =$=
       mapP parallelParams
            (poolGridList 3
                          1
                          (toUnboxed . R.computeS)) =$=
       mapP parallelParams
            (poolGrid (div ((nx header) - (poolingSize params) + 1)
                           (poolingStride params))
                      (div ((nx header) - (poolingSize params) + 1)
                           (poolingStride params))
                      (fisherVector (P.head gmm)
                                    muVar .
                       extractFeaturePoint)) =$=
       featurePointConduit parallelParams =$=
       mergeSource (labelSource $ labelFile params) =$=
       predict (modelName params)
               ((modelName params) P.++ ".out")
