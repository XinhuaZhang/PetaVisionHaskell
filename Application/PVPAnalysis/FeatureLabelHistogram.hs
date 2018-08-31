import           Application.PVP2LibLinear.Conduit
import           Application.PVPAnalysis.Analysis
import           Application.PVPAnalysis.PlotFigure
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.List                          as L
import           Data.Vector                        as V
import           Data.Vector.Unboxed                as VU
import           PetaVision.PVPFile.IO
import           PetaVision.PVPFile.Utility
import           System.Environment
import           System.FilePath

sink
  :: V.Vector (VU.Vector Int)
  -> Sink (Double, PVPOutputData) (ResourceT IO) (V.Vector (VU.Vector Double))
sink vecs = do
  x <- await
  case x of
    Nothing -> return . V.map (VU.map fromIntegral) $ vecs
    Just (label', y) ->
      sink $ V.accum computeFeatureHistogram vecs [(round label', y)]

main = do
  (actFile:groundTruthFile:_) <- getArgs
  actHeader <- readPVPHeader actFile
  groundTruthHead <- readPVPHeader groundTruthFile
  let histVecs =
        V.replicate
          (nf groundTruthHead)
          (VU.replicate (nf actHeader) (0 :: Int))
      s = (nBands actHeader) * (nx actHeader) * (ny actHeader)
  hists <-
    runResourceT $
    pvpFileSource actFile =$= mergeSource (pvpLabelSource groundTruthFile) $$
    sink histVecs
  V.imapM_
    (\i vec ->
       plotHistogram
         (takeBaseName actFile L.++ "_FeatureLabel_" L.++ show i L.++ ".png") .
       VU.map (/ fromIntegral s) $
       vec)
    hists
