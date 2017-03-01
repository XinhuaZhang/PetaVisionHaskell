import           Application.KMeans.ArgsParser     as Parser
import           Application.KMeans.Conduit
import           Application.PVP2LibLinear.Conduit
import           Classifier.LibLinear
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Array.Repa                   as R
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary               as CB
import           Data.Conduit.List                 as CL
import           Data.List                         as L
import           Data.Vector.Unboxed               as VU
import           PetaVision.Data.KMeans            as KMP
import           PetaVision.Data.Pooling
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Array
import           PetaVision.Utility.Conduit
import           PetaVision.Utility.Parallel
import           Prelude                           as P
import           System.Environment
import           System.IO

main = do
  args <- getArgs
  when (L.null args) (error "run with --help to see options.")
  params <- parseArgs args
  header <- readPVPHeader (P.head $ pvpFile params)
  (KMeansModel (KMP.Shape actNy actNx actNf stride) cs kmeansModel) <-
    decodeFile (kmeansFile params)
  let parallelParams =
        ParallelParams (Parser.numThread params) (Parser.batchSize params)
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = (c params)
        , trainNumExamples = nBands header
        , trainFeatureIndexMax =
          (L.length . startPointList . ny $ header) *
          (L.length . startPointList . nx $ header) *
          actNy *
          actNx *
          actNf
        , trainModel = (modelName params)
        }
      startPointList len =
        L.filter
          (\i -> i + (poolingSize params) <= len)
          [0,(poolingStride params) .. len - 1]
  print params
  runResourceT $
    pvpFileSource (P.head $ pvpFile params) $$ CL.map pvpOutputData2Array =$=
    mapP
      parallelParams
      (poolGridList
         (poolingSize params)
         (poolingStride params)
         (toUnboxed . R.computeS)) =$=
    mapP
      parallelParams
      (VU.zip (VU.generate (trainFeatureIndexMax trainParams) id) . computeSoftAssignment kmeansModel) =$=
    trainSink
      trainParams
      [(labelFile params)]
      (Parser.batchSize params)
      (findC params)
