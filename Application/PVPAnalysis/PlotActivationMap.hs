import           Application.PVPAnalysis.Analysis
import           Application.PVPAnalysis.PlotFigure
import           Control.Monad.Trans.Resource
import           Data.Array.Repa                    as R
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.List                          as L
import           Data.Maybe
import           PetaVision.PVPFile.IO
import           Prelude                            as P
import           System.Environment
import           System.FilePath
import Data.Vector.Unboxed as VU
import Text.Printf

main = do
  (filePath:idxStr:_) <- getArgs
  header <- readPVPHeader filePath
  sa <-
    runResourceT $
    pvpFileSource filePath $$
    (do CL.drop ((read idxStr :: Int) - 1)
        x <- await
        case x of
          Nothing -> return ()
          Just y -> yield y) =$=
    CL.take 1
  let vec = toUnboxed . pvpOutputData2Array . L.head $ sa
      nnz = L.length . L.filter (/= 0) . VU.toList $ vec
  printf
    "%s: %.1f%% (%d/%d)\n"
    (takeBaseName filePath)
    ((fromIntegral nnz) / (fromIntegral . VU.length $ vec) * 100 :: Double)
    nnz
    (VU.length vec)
  plotActivation
    (takeBaseName filePath L.++ ".png")
    (printf
       "%.1f%% (%d/%d)"
       ((fromIntegral nnz) / (fromIntegral . VU.length $ vec) * 100 :: Double)
       nnz
       (VU.length vec))
    vec
