module Main where

import           Application.GMM.ConvertPVPGMMConduit
import           Control.Monad                        as M
import           Data.Conduit
import           Data.List                            as L
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel
import           System.Environment
import           System.IO

main = do
  let parallelParams = ParallelParams 8 800
  filePathList <- getArgs
  withBinaryFile
    (L.head filePathList)
    WriteMode
    (\h ->
        M.foldM_
          (\h1 filePath -> do
             header <- readPVPHeader filePath
             M.foldM
               (\handle featureInd ->
                   pvpFileSource filePath $$
                   unpooledSparse2NonsparseConduit parallelParams featureInd =$=
                   hFeatureSink
                     handle
                     (nBands header)
                     (L.product . L.map (\f -> f header) $ [nx, ny]))
               h1
               [0 .. nf header - 1])
          h $
        L.tail filePathList)
