import           Application.Caffe.ArgsParser      as AP
import           Application.Caffe.Conduit
import           Application.Caffe.LMDB
import           Application.PVP2LibLinear.Conduit (pvpLabelSource)
import           Control.Monad                     as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa                   as R
import           Data.Conduit
import           Data.Conduit.List                 as CL
import           Data.List                         as L
import           Data.Vector.Unboxed               as VU
import           PetaVision.Data.Pooling
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
  len' <- runResourceT $ pvpFileSource (L.head . L.head . pvpFile $ params) $$ lenSink 0
  print len'
  len'' <- runResourceT $ (pvpLabelSource . labelFile $ params) $$ lenSink 0
  print len''
  -- print . L.head . pvpFile $ params
  -- header <- readPVPHeader (L.head . pvpFile $ params)
  -- print header
  let parallelParams =
        ParallelParams (AP.numThread params) (AP.batchSize params)
      n = 0 -- 15000
      len = round $ (fromIntegral (len' - n)) * 4 / 5
  print len
  if poolingFlag params
    then if concatFlag params
           then runResourceT $
                (sequenceSources .
                 L.zipWith
                   (\ps xs ->
                      sequenceSources .
                      L.map
                        (\x ->
                           pvpFileSource x =$= dropConduit n =$= takeConduit len =$=
                           poolArrayConduit
                             parallelParams
                             (poolingType params)
                             ps) $
                      xs)
                   (poolingSize params) .
                 pvpFile $
                 params) $$
                concatConduit parallelParams =$=
                mergeSource
                  ((pvpLabelSource . labelFile $ params) =$= dropConduit n) =$=
                saveFloatDataSink
                  ((folderName params) </> "Train" </> "Vector")
                  (PA.batchSize parallelParams)
           else runResourceT $
                (sequenceSources .
                 L.zipWith
                   (\ps xs ->
                      sequenceSources .
                      L.map
                        (\x ->
                           pvpFileSource x =$= takeConduit len =$=
                           poolArrayConduit
                             parallelParams
                             (poolingType params)
                             ps) $
                      xs)
                   (poolingSize params) .
                 pvpFile $
                 params) =$=
                concatArrayConduit parallelParams =$=
                padConduit parallelParams (padLength params) $$
                mergeSource (pvpLabelSource . labelFile $ params) =$=
                saveFloatDataSink
                  ((folderName params) </> "Train" </> "Array")
                  (PA.batchSize parallelParams)
    else if concatFlag params
           then runResourceT $
                (sequenceSources .
                 L.map
                   (sequenceSources .
                    L.map
                      (\x ->
                         pvpFileSource x =$= takeConduit len =$=
                         pvpConduit parallelParams)) .
                 pvpFile $
                 params) $$
                concatConduit parallelParams =$=
                mergeSource (pvpLabelSource . labelFile $ params) =$=
                saveFloatDataSink
                  ((folderName params) </> "Train" </> "Vector")
                  (PA.batchSize parallelParams)
           else runResourceT $
                (sequenceSources .
                 L.map
                   (sequenceSources .
                    L.map
                      (\x ->
                         pvpFileSource x =$= takeConduit len =$=
                         pvpConduit parallelParams)) .
                 pvpFile $
                 params) =$=
                concatArrayConduit parallelParams =$=
                padConduit parallelParams (padLength params) $$
                mergeSource (pvpLabelSource . labelFile $ params) =$=
                saveFloatDataSink
                  ((folderName params) </> "Train" </> "Array")
                  (PA.batchSize parallelParams)
  if poolingFlag params
    then if concatFlag params
           then runResourceT $
                (sequenceSources .
                 L.zipWith
                   (\ps xs ->
                      sequenceSources .
                      L.map
                        (\x ->
                           pvpFileSource x =$= dropConduit (len + n) =$=
                           poolArrayConduit
                             parallelParams
                             (poolingType params)
                             ps) $
                      xs)
                   (poolingSize params) .
                 pvpFile $
                 params) $$
                concatConduit parallelParams =$=
                mergeSource
                  ((pvpLabelSource . labelFile $ params) =$=
                   dropConduit (len + n)) =$=
                saveFloatDataSink
                  ((folderName params) </> "Validate" </> "Vector")
                  (PA.batchSize parallelParams)
           else runResourceT $
                (sequenceSources .
                 L.zipWith
                   (\ps xs ->
                      sequenceSources .
                      L.map
                        (\x ->
                           pvpFileSource x =$= dropConduit len =$=
                           poolArrayConduit
                             parallelParams
                             (poolingType params)
                             ps) $
                      xs)
                   (poolingSize params) .
                 pvpFile $
                 params) =$=
                concatArrayConduit parallelParams =$=
                padConduit parallelParams (padLength params) $$
                mergeSource
                  ((pvpLabelSource . labelFile $ params) =$= dropConduit len) =$=
                saveFloatDataSink
                  ((folderName params) </> "Validate" </> "Array")
                  (PA.batchSize parallelParams)
    else if concatFlag params
           then runResourceT $
                (sequenceSources .
                 L.map
                   (sequenceSources .
                    L.map
                      (\x ->
                         pvpFileSource x =$= dropConduit len =$=
                         pvpConduit parallelParams)) .
                 pvpFile $
                 params) $$
                concatConduit parallelParams =$=
                mergeSource
                  ((pvpLabelSource . labelFile $ params) =$= dropConduit len) =$=
                saveFloatDataSink
                  ((folderName params) </> "Validate" </> "Vector")
                  (PA.batchSize parallelParams)
           else runResourceT $
                (sequenceSources .
                 L.map
                   (sequenceSources .
                    L.map
                      (\x ->
                         pvpFileSource x =$= dropConduit len =$=
                         pvpConduit parallelParams)) .
                 pvpFile $
                 params) =$=
                concatArrayConduit parallelParams =$=
                padConduit parallelParams (padLength params) $$
                mergeSource
                  ((pvpLabelSource . labelFile $ params) =$= dropConduit len) =$=
                saveFloatDataSink
                  ((folderName params) </> "Validate" </> "Array")
                  (PA.batchSize parallelParams)
