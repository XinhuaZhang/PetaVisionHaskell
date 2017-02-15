import           Application.GMM.Gaussian
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Codec.Picture
import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           PetaVision.Data.Weight
import           PetaVision.PVPFile.IO
import           System.Environment
import           Application.PlotWeight.Grid
import Data.Vector.Unboxed as VU

main =
  do (gmmFile:weightFile:_) <- getArgs
     (gmm:_) <- readGMM gmmFile :: IO [GMM]
     weight <- runResourceT $ pvpFileSource weightFile $$ CL.head
     case weight of
       Nothing -> error "Read weight error"
       Just (PVP_OUTPUT_KERNEL w) ->
         do let ys =
                  L.map (\(Model (ww,gm)) ->
                           let (Z :. _ :. _ :. _ :. numPatches') = extent w
                               muVec =
                                 fromUnboxed (Z :. numPatches') . gaussianMu $
                                 gm
                               weightedArr =
                                 sumS . R.map (* ww) $
                                 traverse2 
                                           (normalizeWeight (fromIntegral (maxBound :: Pixel8))
                                                            w)
                                           muVec
                                           const
                                           (\fw fmu idx@(Z :. _j :. _i :. _k :. n) ->
                                              fw idx * fmu (Z :. n))
                           in weightedArr)
                        (model gmm)
            M.mapM_ (\(i,(Model (w,gm)),y) ->
                       do print (i,w,VU.zip (gaussianMu gm) (gaussianSigma2 gm))
                          plotWeightPatch (show i L.++ ".png")
                                          y) .
              L.filter (\(i,(Model (w,_)),y) -> w > 0.01) $
              L.zip3 [1 ..]
                     (model gmm)
                     ys
            savePngImage
              "dictionary.png"
              (getGridImage $
               normalizeWeight (fromIntegral (maxBound :: Pixel8))
                               w)
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
