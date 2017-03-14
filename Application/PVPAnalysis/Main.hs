module Main where

import           Application.PVPAnalysis.Analysis
import           Application.PVPAnalysis.PlotFigure
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           PetaVision.PVPFile.IO
import           Prelude                            as P
import           System.Environment

main =
  do (filePath:name:_) <- getArgs
     header <- readPVPHeader filePath
     sa <-
       runResourceT $
       pvpFileSource filePath $$ sparsityActivationHistogramSink header
     plotSparsityNHistogram (name ++ "ActivationHistogram.png") sa
