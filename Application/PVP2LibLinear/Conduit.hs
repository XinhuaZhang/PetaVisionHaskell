module Application.PVP2LibLinear.Conduit
  (trainSink
  ,concatConduit
  ,predictConduit
  ,concatPooledConduit)
  where

import           Classifier.LibLinear.Bindings
import           Classifier.LibLinear.Example
import           Classifier.LibLinear.Interface
import           Control.DeepSeq
import           Control.Monad                  as M
import           Control.Monad.IO.Class
import           Control.Parallel.Strategies
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.Maybe
import           Data.Maybe                     as Maybe
import           Data.Vector.Unboxed            as VU
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           PetaVision.PVPFile.IO
import           PetaVision.PVPFile.Pooling
import           Prelude                        as P

getFeaturePtr
  :: [(Int,Double)] -> IO (Ptr C'feature_node)
getFeaturePtr xs =
  newArray (pairs P.++
            [C'feature_node (-1)
                            0])
  where pairs =
          P.map (\(i,x) ->
                   C'feature_node (P.fromIntegral i)
                                  (realToFrac x))
                xs

trainSink
  :: TrainParams -> FilePath -> Sink [(Int,Double)] IO ()
trainSink params filePath =
  do label <- liftIO $ readLabelFile filePath
     featurePtr <-
       CL.foldM (\xs x ->
                   do ptr <- liftIO $ getFeaturePtr x
                      return (ptr : xs))
                []
     liftIO $ train params label $ P.reverse featurePtr

-- liblinear feature node's index starts from 1 !!!!!
concatConduit :: [Int] -> Conduit [PVPOutputData] IO [(Int,Double)]
concatConduit offset =
  awaitForever
    (yield .
     P.concat .
     P.zipWith (\i x -> P.map (\(ind,val) -> (ind + i + 1,val)) x) offset .
     P.map parsePVPOutputData)
  where parsePVPOutputData
          :: PVPOutputData -> [(Int,Double)]
        parsePVPOutputData (PVP_NONSPIKING_ACT xs) =
          P.filter (\(i,v) -> v /= 0) $
          P.zipWith (\i v -> (i,v))
                    [1 ..]
                    xs
        parsePVPOutputData (PVP_ACT_SPARSEVALUES xs) = xs
        parsePVPOutputData _ = error "Doesn't support this type of pvpfile."

concatPooledConduit
  :: Conduit [VU.Vector (Int,Double)] IO [(Int,Double)]
concatPooledConduit = awaitForever (yield . VU.toList . VU.concat)

predictConduit
  :: Conduit [(Int,Double)] IO (Ptr C'feature_node)
predictConduit =
  awaitForever
    (\x ->
       do ptr <- liftIO $ getFeaturePtr x
          yield ptr)
