{-# LANGUAGE BangPatterns #-}
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
import qualified Control.Monad.Parallel         as MP
import           Control.Parallel.Strategies
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Maybe
import           Data.Maybe                     as Maybe
import           Data.Vector.Unboxed            as VU
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           PetaVision.PVPFile.IO
import           PetaVision.Data.Pooling
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
  :: TrainParams -> FilePath -> Int -> Sink [(Int,Double)] IO ()
trainSink params filePath batchSize =
  do label <- liftIO $ readLabelFile filePath
     featurePtr <- go []
     liftIO $ train params label featurePtr
  where go ptrs =
          do xs <- CL.take batchSize
             if P.length xs > 0
                then do let !norm = P.map (sqrt . L.foldl' (\a b -> a + b^2) 0 . snd . P.unzip) xs
                            ys = P.zipWith (\x n -> P.map (\(i,v) -> (i, v / n)) x) xs norm
                        featurePtr <- liftIO $ MP.mapM getFeaturePtr ys
                        go $ ptrs P.++ featurePtr
                else return ptrs

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
        parsePVPOutputData (PVP_OUTPUT_NONSPIKING_ACT _ xs) =
          P.filter (\(i,v) -> v /= 0) $
          P.zipWith (\i v -> (i,v))
                    [1 ..]
                    xs
        parsePVPOutputData (PVP_OUTPUT_ACT_SPARSEVALUES _ xs) = xs
        parsePVPOutputData _ = error "Doesn't support this type of pvpfile."

concatPooledConduit
  :: Conduit [VU.Vector (Int,Double)] IO [(Int,Double)]
concatPooledConduit = awaitForever (yield . VU.toList . VU.concat)

predictConduit
  :: Conduit [(Int,Double)] IO (Ptr C'feature_node)
predictConduit =
  awaitForever
    (\x ->
       do let !norm = sqrt . L.foldl' (\a b -> a + b^2) 0 . snd . P.unzip $ x
              y = P.map (\(i,v) -> (i, v / norm)) x
          ptr <- liftIO $ getFeaturePtr y
          yield ptr)
