module Main where

import           Application.PVP2LibLinear.ArgsParser as AP
import           Application.PVP2LibLinear.Conduit
import           Application.PVP2LibLinear.Utility
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           PetaVision.PVPFile.IO
import           PetaVision.Data.Pooling
import           PetaVision.Utility.Parallel          as PA
import           Prelude                              as P
import           System.Environment

main =
  do args <- getArgs
     if null args
        then error "run with --help to see options."
        else return ()
     params <- parseArgs args
     print params
     header <-
       M.mapM (readPVPHeader . P.head)
              (pvpFile params)
     batchHeader <- M.mapM readPVPHeader . P.head . pvpFile $ params
     let source =
           P.map (\filePath ->
                    (sequenceSources . P.map pvpFileSource $ filePath) =$=
                    CL.concat)
                 (pvpFile params)
         nbands = P.sum . P.map nBands $ batchHeader
         dims = dimOffset header
         trainParams =
           TrainParams {trainSolver = L2R_L2LOSS_SVC_DUAL
                       ,trainC = (c params)
                       ,trainNumExamples = nbands
                       ,trainFeatureIndexMax =
                          (\((nf,ny,nx),n) -> n + nf * ny * nx) . P.last $ dims
                       ,trainModel = (modelName params)}
     print trainParams
     if poolingFlag params
        then do putStrLn $
                  "Using CPU for " ++ show (poolingType params) ++ " Pooling"
                sequenceSources
                  (P.zipWith (\s offset ->
                                s =$=
                                poolVecConduit
                                  (ParallelParams (AP.numThread params)
                                                  (AP.batchSize params))
                                  (poolingType params)
                                  (poolingSize params)
                                  offset)
                             source
                             (snd . unzip $ dims)) $$
                  concatPooledConduit =$
                  trainSink trainParams
                            (labelFile params)
                            (AP.batchSize params)
                            (findC params)
        else sequenceSources source $$ concatConduit (snd . unzip $ dims) =$
             trainSink trainParams
                       (labelFile params)
                       (AP.batchSize params)
                       (findC params)
