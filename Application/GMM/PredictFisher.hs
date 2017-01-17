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
import           PetaVision.Utility.Parallel  as Parallel
import           Prelude                      as P
import           System.Environment

featurePointConduit :: Conduit (VU.Vector Double) (ResourceT IO) (Ptr C'feature_node)
featurePointConduit =
  awaitForever
    (\x ->
       do y <- liftIO . getFeatureVecPtr . Dense . VU.toList $ x
          yield y)

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
       featurePointConduit =$=
       mergeSource (labelSource $ labelFile params) =$=
       predict (modelName params)
               ((modelName params) P.++ ".out")
