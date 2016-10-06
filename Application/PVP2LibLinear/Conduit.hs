module Application.PVP2LibLinear.Conduit
  (trainSink
  ,concatConduit
  ,predictConduit
  ,concatPooledConduit)
  where

import           Classifier.LibLinear.Bindings
import           Classifier.LibLinear.Example
import           Classifier.LibLinear.Interface
import           Control.Monad.IO.Class
import           Data.Array.Unboxed             as AU
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           PetaVision.PVPFile.IO
import           PetaVision.PVPFile.Pooling
import           Prelude                        as P

getFeaturePtr
  :: [(Int,Double)] -> IO (Ptr C'feature_node)
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
          P.zipWith (\i v -> (i,v))
                    [1 ..]
                    xs
        parsePVPOutputData (PVP_ACT_SPARSEVALUES xs) = xs
        parsePVPOutputData _ = error "Doesn't support this type of pvpfile."

concatPooledConduit :: [Int] -> Conduit [AU.Array (Int,Int,Int) Double] IO [(Int,Double)]
concatPooledConduit offset =
  awaitForever
    (yield .
     P.concat .
     P.zipWith (\i x -> P.map (\(ind,val) -> (ind + i,val)) x) offset .
     P.map (P.zip [1 ..] . elems))

predictConduit
  :: Conduit [(Int,Double)] IO (Ptr C'feature_node)
predictConduit =
  awaitForever
    (\x ->
       do ptr <- liftIO $ getFeaturePtr x
          yield ptr)
