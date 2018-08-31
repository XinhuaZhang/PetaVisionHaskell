import           Application.PVPAnalysis.Analysis
import           Application.PVPAnalysis.PlotFigure
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List                      as CL
import           Data.List                              as L
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           PetaVision.PVPFile.IO
import           Prelude                                as P
import           System.Environment

pad0
  :: (Show a)
  => Int -> a -> String
pad0 n x
  | len < n = L.replicate (n - len) '0'  L.++ y
  | otherwise = y
  where y = show x
        len = L.length y

weightConduit :: Conduit FilePath (ResourceT IO) PVPOutputData
weightConduit =
  awaitForever
    (\x -> do
       w <- liftIO . runResourceT $ pvpFileSource x $$ CL.consume
       yield . L.head $ w
       weightConduit)

main = do
  (folderPath:weightFileName:intervalStr:nStr:numLenStr:_) <- getArgs
  let n = read nStr :: Int
      interval = read intervalStr :: Int
      numLen = read numLenStr :: Int
      pathList =
        L.map
          (\i ->
             folderPath L.++ "/" L.++ "Checkpoint" L.++
             (pad0 numLen (i * interval)) L.++
             "/" L.++
             weightFileName)
          [0 .. div n interval]
  wd <-
    runResourceT $
    CL.sourceList pathList $$ weightConduit =$= weightChangeConduit =$=
    CL.consume
  toFile def ("ChangeOfWeight_" L.++ weightFileName L.++ ".png") $ do
    layout_title .= "Mean Square Difference of Weights"
    plot (line "" [P.zip [(0 :: Int) ..] wd])
