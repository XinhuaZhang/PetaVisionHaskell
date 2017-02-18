import           Application.KMeans.ArgsParser   as Parser
import           Application.KMeans.Conduit
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.Binary          as CB
import           Data.Conduit.List            as CL
import           PetaVision.Data.Pooling
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Array
import           PetaVision.Utility.Conduit
import           PetaVision.Utility.Parallel
import           Prelude                      as P
import           System.Environment
import           System.IO

main =
  do args <- getArgs
     if null args
        then error "run with --help to see options."
        else return ()
     params <- parseArgs args
     let parallelParams =
           ParallelParams (Parser.numThread params)
                          (Parser.batchSize params)
     print params
     runResourceT
       (pvpFileSource (P.head $ pvpFile params) $$
        -- poolArrayConduit
        --   (ParallelParams (Parser.numThread params) (Parser.batchSize params))
        --   (poolingType params)
        --   (poolingSize params)
        --   undefined =$=
        CL.map pvpOutputData2Array =$=
        mapP parallelParams (poolGridList 3 1 (toUnboxed . R.computeS)) =$=
        kmeansVecSinkP parallelParams (kmeansFile params) (numGaussian params) 500 1 (threshold params)) 
        -- kmeansVecSink (kmeansFile params) (numGaussian params) 1)
        -- kmeansArrSinkP parallelParams (kmeansFile params) (numGaussian params) 100 1 (threshold params)) 

