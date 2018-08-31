import           Application.Caffe.ArgsParser      as AP
import           Application.Caffe.Conduit
import           Application.Caffe.LMDB
import           Application.PVP2LibLinear.Conduit (pvpLabelSource)
import           Classifier.LibLinear.Example      (readLabelFile)
import           Control.Monad                     as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa                   as R
import           Data.Conduit
import           Data.Conduit.List                 as CL
import           Data.List                         as L
import           Data.Vector.Unboxed               as VU
import           PetaVision.Data.Pooling
import           PetaVision.PVPFile.IO
-- import           PetaVision.Utility.HDF5
import           PetaVision.Utility.Parallel       as PA
import           System.Environment
import           System.FilePath

main = do
  args <- getArgs
  if L.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  print params
  let parallelParams =
        ParallelParams (AP.numThread params) (AP.batchSize params)
  labels <-
    if L.isSuffixOf ".pvp" . labelFile $ params
      then runResourceT $ (pvpLabelSource . labelFile $ params) $$ CL.consume
      else readLabelFile . labelFile $ params
  if poolingFlag params
    then if concatFlag params
           then runResourceT $
                (sequenceSources .
                 L.zipWith
                   (\ps xs ->
                      sequenceSources .
                      L.map
                        (\x ->
                           pvpFileSource x =$=
                           poolArrayConduit
                             parallelParams
                             (poolingType params)
                             ps) $
                      xs)
                   (poolingSize params) .
                 pvpFile $
                 params) $$
                concatConduit parallelParams =$=
                mergeSource (sourceList labels) =$=
                saveFloatDataSink
                  ((folderName params) </> (modelName params) </> "Vector")
                  (PA.batchSize parallelParams)
           else runResourceT $
                (sequenceSources .
                 L.zipWith
                   (\ps xs ->
                      sequenceSources .
                      L.map
                        (\x ->
                           pvpFileSource x =$=
                           poolArrayConduit
                             parallelParams
                             (poolingType params)
                             ps) $
                      xs)
                   (poolingSize params) .
                 pvpFile $
                 params) $$
                concatArrayConduit parallelParams =$=
                padConduit parallelParams (padLength params) =$=
                mergeSource (sourceList labels) =$=
                saveFloatDataSink
                  ((folderName params) </> (modelName params) </> "Array")
                  (PA.batchSize parallelParams)
    else if concatFlag params
           then runResourceT $
                (sequenceSources .
                 L.map
                   (sequenceSources .
                    L.map (\x -> pvpFileSource x =$= pvpConduit parallelParams)) .
                 pvpFile $
                 params) $$
                concatConduit parallelParams =$=
                mergeSource (sourceList labels) =$=
                saveFloatDataSink
                  ((folderName params) </> (modelName params) </> "Vector")
                  (PA.batchSize parallelParams)
           else runResourceT $
                (sequenceSources .
                 L.map
                   (sequenceSources .
                    L.map (\x -> pvpFileSource x =$= pvpConduit parallelParams)) .
                 pvpFile $
                 params) $$
                concatArrayConduit parallelParams =$=
                padConduit parallelParams (padLength params) =$=
                mergeSource (sourceList labels) =$=
                saveFloatDataSink
                  ((folderName params) </> (modelName params) </> "Array")
                  (PA.batchSize parallelParams)
