import           Codec.Picture
import           Control.Monad                as M
--import           Control.Monad.Parallel       as MP
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Maybe                   as Maybe
import           PetaVision.Data.Weight
import           PetaVision.PVPFile.IO
import           Prelude                      as P
import           System.Directory
import           System.Environment

main = do
  args <- getArgs -- nth_recon:numWeight:reconFile:weightFile1:weightFile2:...
  let n = read . L.head $ args :: Int
      numWeight = read . L.head . L.tail $ args
      (weightFileList, strideList) =
        L.splitAt (numWeight + 1) . L.tail . L.tail $ args
      name = L.last strideList
  recons <- runResourceT $ pvpFileSource (L.head weightFileList) $$ CL.take n
  weights <-
    M.mapM (\reconFile -> runResourceT $ pvpFileSource reconFile $$ CL.head) .
    L.tail $
    weightFileList
  dir <- getCurrentDirectory
  createDirectoryIfMissing True (dir P.++ "/Recon")
  if L.any isNothing weights || L.null recons
    then error "Read recon error"
    else let weightArrList =
               L.map ((\(PVP_OUTPUT_KERNEL x) -> x) . fromJust) weights
             reconArr =
               (\x ->
                  case x of
                    PVP_OUTPUT_ACT _ _ -> undefined
                    PVP_OUTPUT_NONSPIKING_ACT (PVPDimension nx ny nf) arr' ->
                      R.fromUnboxed (Z :. ny :. nx :. nf :. (1 :: Int)) arr'
                    PVP_OUTPUT_ACT_SPARSEVALUES _ _ -> undefined
                    PVP_OUTPUT_KERNEL _ -> undefined) .
               L.last $
               recons
             result =
               weightListReconstruction
                 (reconArr : weightArrList)
                 (L.map (\x -> read x :: Int) . L.init $ strideList)
         in plotWeightPatch (dir P.++ "/Recon/" P.++ name P.++ "Recon.png") .
            computeS . R.slice result $
            (Z :. All :. All :. All :. (0 :: Int))
