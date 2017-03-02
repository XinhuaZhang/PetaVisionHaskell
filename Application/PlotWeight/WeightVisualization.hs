import           Application.PlotWeight.Grid
import           Codec.Picture
import           Data.Conduit
import           Data.Conduit.List           as CL
import           PetaVision.Data.Weight
import           PetaVision.PVPFile.IO
import           Prelude                     as P
import           System.Directory
import           System.Environment
import           Control.Monad.Trans.Resource

main =
  do (weightFile:folderName:_) <- getArgs
     weight <- runResourceT $ pvpFileSource weightFile $$ CL.head
     dir <- getCurrentDirectory
     removePathForcibly (dir P.++ "/Weight/" P.++ folderName)
     createDirectoryIfMissing True
                              (dir P.++ "/Weight/" P.++ folderName)
     case weight of
       Nothing -> error "Read weight error"
       Just x ->
         do let w = (\(PVP_OUTPUT_KERNEL y) -> y) x
                normalizedW =
                  normalizeWeight (P.fromIntegral (maxBound :: Pixel8))
                                  w
            weightVisualization (dir P.++ "/Weight/" P.++ folderName)
                                w
            savePngImage
              (dir P.++ "/Weight/" P.++ folderName P.++ "/dictionary.png")
              (getGridImage normalizedW)
