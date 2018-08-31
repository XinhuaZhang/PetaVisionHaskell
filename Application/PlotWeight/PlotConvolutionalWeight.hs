{-# LANGUAGE FlexibleContexts #-}
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
import           PetaVision.Utility.Parallel
import           System.Directory
import           System.Environment

main = do
  args <- getArgs -- numWeight: WeightFiles : StrideList : ActFileName : NameStr : NumThread
  let numWeight = read . L.head $ args
      (weightFileList, strideList) = L.splitAt numWeight . L.tail $ args
  print weightFileList
  print strideList
  weightList <-
    M.mapM
      (\path -> runResourceT $ pvpFileSource path $$ CL.head)
      weightFileList
  let parallelParams = ParallelParams (read . L.last $ strideList :: Int) 80
      extracFunc = (\(PVP_OUTPUT_KERNEL x) -> x) . fromJust
      -- extracFunc1 = (\(PVP_OUTPUT_KERNEL x) -> let (Z :. a :. b :. c :. _)  = extent x
      --                                          in computeS . backpermute (Z :. a :. b :. c :. 16) id $ x) . fromJust
      name = L.last . L.init $ strideList
      actFileName = L.last . L.init . L.init $ strideList
      w =
        weightListReconstruction
          parallelParams
          (L.map extracFunc weightList)
          -- ((extracFunc1 . L.head $ weightList) : (L.map extracFunc . L.tail $  weightList))
          (L.map (\x -> read x :: Int) . L.init . L.init . L.init $ strideList)
  header <- readPVPHeader actFileName
  sa <-
    runResourceT $
    pvpFileSource actFileName $$ sparsityActivationHistogramSink header
  let idx = L.reverse . fst . L.unzip . L.sortOn snd . L.zip [0 ..] . VU.toList . snd $ sa
      -- idx = [0..(VU.length . snd $ sa) - 1]
  createDirectoryIfMissing True "Weight"
  plotWeight "Weight" w
  savePngImage (name L.++ ".png") (getGridImage . reorderWeight idx
                                   $ w) 
