module Application.PVP2LibLinear.Conduit
       (trainSink, concatConduit, predictConduit) where

import           Classifier.LibLinear.Bindings
import           Classifier.LibLinear.Example
import           Classifier.LibLinear.Interface
import           Control.Monad.IO.Class
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Prelude                        as P

getFeaturePtr :: [(Int,Double)] -> IO (Ptr C'feature_node)
getFeaturePtr xs =
  newArray (pairs ++
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
     feature <- consume
     featurePtr <- liftIO $ P.mapM getFeaturePtr feature
     liftIO $ train params label featurePtr

-- liblinear feature node's index starts from 1 !!!!!
concatConduit
  :: [Int] -> Conduit [[(Int,Double)]] IO [(Int,Double)]
concatConduit offset =
  awaitForever
    (yield . P.concat .
     P.zipWith (\i x -> P.map (\(ind,val) -> (ind + i + 1,val)) x) offset)

predictConduit
  :: Conduit [(Int,Double)] IO (Ptr C'feature_node)
predictConduit =
  awaitForever
    (\x ->
       do ptr <- liftIO $ getFeaturePtr x
          yield ptr)
