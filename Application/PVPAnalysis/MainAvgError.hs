module Main where

import           Application.PVPAnalysis.Analysis
import           Control.Monad.Trans.Resource
import           Data.Conduit
import PetaVision.PVPFile.IO
import           System.Environment

main = do
  (filePath:_) <- getArgs
  header <- readPVPHeader filePath
  runResourceT $ pvpFileSource filePath $$ averageError header
