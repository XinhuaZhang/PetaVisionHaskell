import           Application.KMeans.ArgsParser as Parser
import           Application.KMeans.Conduit
import           Application.PlotWeight.Grid
import           Codec.Picture
import           Control.Monad                 as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa               as R
import           Data.Conduit
import           Data.Conduit.List             as CL
import           Data.List                     as L
import           Data.Vector.Unboxed           as VU
import           PetaVision.Data.Weight
import           PetaVision.PVPFile.IO
import           System.Environment

main = do
  (kmeansFile:weightFile:_) <- getArgs
  kmeansModel <- readKMeansModel kmeansFile
  weight <- runResourceT $ pvpFileSource weightFile $$ CL.head
  case weight of
    Nothing -> error "Read weight error"
    Just (PVP_OUTPUT_KERNEL w) -> do
      let ys =
            L.map
              (\zs ->
                  let vec = fromListUnboxed (Z :. (L.length zs)) zs
                  in sumS $
                     traverse2
                       w
                       vec
                       const
                       (\fw fmu idx@(Z :. _j :. _i :. _k :. n) ->
                           fw idx * fmu (Z :. n)))
              kmeansModel
      savePngImage
        "dictionary.png"
        (getGridImage $ normalizeWeight (fromIntegral (maxBound :: Pixel8)) w)
      savePngImage
        "mean.png"
        (getGridImage .
         normalizeWeight (fromIntegral (maxBound :: Pixel8)) .
         computeS .
         L.foldl1' R.append .
         L.map (R.extend (Z :. All :. All :. All :. (1 :: Int))) $
         ys)
      plotWeightPatch
        "sum.png"
        (sumS .
         L.foldl1' R.append .
         L.map (R.extend (Z :. All :. All :. All :. (1 :: Int))) $
         ys)
