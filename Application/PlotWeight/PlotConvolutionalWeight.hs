{-# LANGUAGE FlexibleContexts #-}
import           Application.PlotWeight.Grid
import           Codec.Picture
import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Maybe
import           PetaVision.Data.Convolution
import           PetaVision.Data.Weight
import           PetaVision.PVPFile.IO
import           System.Environment

main =
  do args <- getArgs
     let numWeight = read . L.head $ args
         (weightFileList,strideList) = L.splitAt numWeight . L.tail $ args
     print weightFileList
     print strideList
     weightList <-
       M.mapM (\path -> runResourceT $ pvpFileSource path $$ CL.head) weightFileList
     let extracFunc = (\(PVP_OUTPUT_KERNEL x) -> x) . fromJust
         w =
           weightListReconstruction
             (L.map extracFunc weightList)
             (L.map (\x -> read x :: Int) . L.init $ strideList)
         name = L.last strideList
     savePngImage (name L.++ ".png")
                  (getGridImage w)
