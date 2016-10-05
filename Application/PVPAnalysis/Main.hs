module Main where

import           Application.PVPAnalysis.Analysis
import           Data.Conduit
import           PetaVision.PVPFile.IO
import           Prelude                          as P
import           System.Environment

main =
  do (filePath:_) <- getArgs
     header <- readPVPHeader filePath
     pvpFileSource filePath $$ sparsity header
