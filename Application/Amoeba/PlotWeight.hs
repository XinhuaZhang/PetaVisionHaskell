import           Application.PlotWeight.Grid
import           Application.PVPAnalysis.Analysis
import           Codec.Picture
import           Control.Monad                    as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa                  as R
import           Data.Conduit
import           Data.Conduit.List                as CL
import           Data.List                        as L
import           Data.Maybe
import           Data.Vector.Unboxed              as VU
import           PetaVision.Data.Convolution
import           PetaVision.Data.Weight
import           PetaVision.PVPFile.IO
import           System.Directory
import           System.Environment

main = do
  (weightFilePath:dictName:_) <- getArgs
  weight <-
    (\(PVP_OUTPUT_KERNEL x) -> x) . fromJust <$>
    (runConduitRes $ pvpFileSource weightFilePath .| CL.head)
  let (Z :. _ :. _ :. nf :. _) = extent weight
  createDirectoryIfMissing True "Weight"
  M.mapM_
    (\i ->
       let normalizedW =
             normalizeWeight (fromIntegral (maxBound :: Pixel8)) .
             computeS .
             extend (Z :. All :. All :. (1 :: Int) :. All) . R.slice weight $
             (Z :. All :. All :. i :. All)
       in savePngImage
            ("Weight/" L.++ dictName L.++ "_" L.++ show i L.++ ".png")
            (getGridImage normalizedW))
    [0 .. nf - 1]
