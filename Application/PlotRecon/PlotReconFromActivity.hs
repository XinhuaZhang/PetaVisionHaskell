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

takeSink :: Int -> Sink e (ResourceT IO) e
takeSink n = do
  CL.drop (n - 1)
  x <- await
  case x of
    Nothing -> error "takeSink: no element"
    Just y -> return y

main = do
  (actFileName:ataWeightFileName:strideStr:indexStr:fileName:_) <- getArgs
  let index = read indexStr :: Int
  act <- runResourceT $ pvpFileSource actFileName $$ takeSink index
  weight <- runResourceT $ pvpFileSource ataWeightFileName $$ CL.head
  let recon =
        weightReconstructionAct
          (pvpOutputData2Array act)
          ((\(PVP_OUTPUT_KERNEL x) -> x) . fromJust $ weight)
          (read strideStr :: Int)
  plotWeightPatch ("Recon/" L.++ fileName L.++ ".png") recon
