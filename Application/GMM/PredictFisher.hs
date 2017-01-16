module Main where

import           Application.GMM.ArgsParser   as Parser
import           Application.GMM.Conduit
import           Application.GMM.FisherKernel
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Classifier.LibLinear
import           Classifier.LibLinear.Example
import           Control.Monad.IO.Class
import           Control.Monad.Parallel       as MP
import           Data.Array.Repa              as R
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Time
import           Data.Vector.Unboxed          as VU
import           Foreign.Ptr
import           PetaVision.Data.Pooling
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel  as Parallel
import           Prelude                      as P
import           System.Environment

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
     pvpFileSource (P.head $ pvpFile params) $$
       featureConduit parallelParams =$=
       fisherVectorConduit parallelParams gmm =$=
       CL.mapM (getFeatureVecPtr . Dense . VU.toList) =$=
       mergeSource (labelSource $ labelFile params) =$=
       predict (modelName params)
               ((modelName params) P.++ ".out")
