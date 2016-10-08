module Main where

import           Application.PVP2LibLinear.ArgsParser as AP
import           Application.PVP2LibLinear.Conduit
import           Application.PVP2LibLinear.Utility
import           Classifier.LibLinear
import           Control.Monad                        as M
import           CUDA.MultiGPU
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           PetaVision.PVPFile.IO
import           PetaVision.PVPFile.Pooling
import           PetaVision.Utility.Parallel          as PA
import           Prelude                              as P
import           System.Environment

main =
  do args <- getArgs
     if null args
        then error "run with --help to see options."
        else return ()
     params <- parseArgs args
     header <- M.mapM readPVPHeader (pvpFile params)
     let source = P.map pvpFileSource (pvpFile params)
         nbands = (nBands $ P.head header)
         dims = dimOffset header
         trainParams =
           TrainParams {trainSolver = L2R_L2LOSS_SVC_DUAL
                       ,trainC = (c params)
                       ,trainNumExamples = nbands
                       ,trainFeatureIndexMax =
                          (\((nf,ny,nx),n) -> n + nf * ny * nx) . P.last $ dims
                       ,trainModel = (modelName params)}
     if poolingFlag params
        then do if gpuPoolingFlag params
                   then putStrLn "Using GPU for " ++ show (poolingType params) ++ " Pooling"
                   else putStrLn "Using CPU for " ++ show (poolingType params) ++ " Pooling"
                ctx <- initializeGPUCtx (Option $ gpuId params)
                sequenceSources
                  (P.zipWith3
                     (\s h offset ->
                        s =$=
                        if gpuPoolingFlag params
                           then poolAccConduit GPUFloat
                                               ctx
                                               (poolingType params)
                                               (AP.batchSize params)
                                               (ny h,nx h,nf h)
                                               offset
                           else poolConduit
                                  (ParallelParams (AP.numThread params)
                                                  (AP.batchSize params))
                                  (poolingType params)
                                  (poolingSize params)
                                  (ny h,nx h,nf h)
                                  offset)
                     source
                     header
                     (snd . unzip $ dims)) $$
                  concatPooledConduit =$=
                  predictConduit =$=
                  mergeSource (labelSource $ labelFile params) =$=
                  predict (modelName params) "result.txt"
                destoryGPUCtx ctx
        else sequenceSources source $$ concatConduit (snd . unzip $ dims) =$=
             predictConduit =$=
             mergeSource (labelSource $ labelFile params) =$=
             predict (modelName params) "result.txt"
