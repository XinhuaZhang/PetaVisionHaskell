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
import           Control.Monad.Trans.Resource

main =
  do args <- getArgs
     if null args
        then error "run with --help to see options."
        else return ()
     params <- parseArgs args
     header <-
       M.mapM (readPVPHeader . P.head)
              (pvpFile params)
     let source =
           P.map (\filePath ->
                    (sequenceSources . P.map pvpFileSource $ filePath) =$=
                    CL.concat)
                 (pvpFile params)
         dims = dimOffset header
     if poolingFlag params
        then do putStrLn $
                  "Using CPU for " ++ show (poolingType params) ++ " Pooling"
                runResourceT $
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
                  predictConduit =$=
                  mergeSource
                    (sequenceSources
                       (if L.isSuffixOf ".pvp" . L.head . labelFile $ params
                           then (P.map pvpLabelSource $ labelFile params)
                           else (P.map labelSource $ labelFile params)) =$=
                     CL.concat) =$=
                  predict (modelName params) "result.txt"
        else runResourceT $
             sequenceSources source $$ concatConduit (snd . unzip $ dims) =$=
             predictConduit =$=
             mergeSource
               (sequenceSources
                  (if L.isSuffixOf ".pvp" . L.head . labelFile $ params
                      then (P.map pvpLabelSource $ labelFile params)
                      else (P.map labelSource $ labelFile params)) =$=
                CL.concat) =$=
             predict (modelName params) "result.txt"
