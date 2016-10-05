module Main where

import           Application.PVP2LibLinear.ArgsParser
import           Application.PVP2LibLinear.Conduit
import           Application.PVP2LibLinear.Utility
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           PetaVision.PVPFile.IO
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
         nbands = (P.head header) !! 17
         dims = dimOffset header
         trainParams =
           TrainParams {trainSolver = L2R_L2LOSS_SVC_DUAL
                       ,trainC = (c params)
                       ,trainNumExamples = nbands
                       ,trainFeatureIndexMax =
                          (\((nf,ny,nx),n) -> n + nf * ny * nx) . P.last $ dims
                       ,trainModel = (modelName params)}
     sequenceSources source $$ concatConduit (snd . unzip $ dims) =$=
       predictConduit =$=
       mergeSource (labelSource $ labelFile params) =$=
       predict (modelName params) "result.txt"
