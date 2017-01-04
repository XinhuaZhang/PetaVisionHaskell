module Main where

import           Application.PVP2LibLinear.ArgsParser as AP
import           Application.PVP2LibLinear.Conduit
import           Application.PVP2LibLinear.Utility
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           PetaVision.Data.Pooling
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel          as PA
import           Prelude                              as P
import           System.Environment

main = do
  args <- getArgs
  if null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  header <- M.mapM (readPVPHeader . P.head) (pvpFile params)
  let source = P.map (sequenceSources . P.map pvpFileSource) (pvpFile params)
      nbands = (nBands $ P.head header)
      dims = dimOffset header
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = (c params)
        , trainNumExamples = nbands
        , trainFeatureIndexMax =
          (\((nf, ny, nx), n) -> n + nf * ny * nx) . P.last $ dims
        , trainModel = (modelName params)
        }
  if poolingFlag params
    then do
      putStrLn $ "Using CPU for " ++ show (poolingType params) ++ " Pooling"
      sequenceSources
        (P.zipWith3
           (\s h offset ->
               s =$= CL.concat =$=
               poolVecConduit
                 (ParallelParams (AP.numThread params) (AP.batchSize params))
                 (poolingType params)
                 (poolingSize params)
                 offset)
           source
           header
           (snd . unzip $ dims)) $$
        concatPooledConduit =$
        predictConduit =$=
        mergeSource
          (sequenceSources
             (if L.isSuffixOf ".pvp" . L.head . labelFile $ params
                then (P.map pvpLabelSource $ labelFile params)
                else (P.map labelSource $ labelFile params)) =$=
           CL.concat) =$=
        predict (modelName params) "result.txt"
    else sequenceSources source $$ CL.concat =$=
         concatConduit (snd . unzip $ dims) =$=
         predictConduit =$=
         mergeSource
           (sequenceSources
              (if L.isSuffixOf ".pvp" . L.head . labelFile $ params
                 then (P.map pvpLabelSource $ labelFile params)
                 else (P.map labelSource $ labelFile params)) =$=
            CL.concat) =$=
         predict (modelName params) "result.txt"
