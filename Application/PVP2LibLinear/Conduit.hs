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
import           PetaVision.Data.Pooling
import           PetaVision.PVPFile.IO
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

trainSink :: TrainParams
          -> FilePath
          -> Int
          -> Sink (VU.Vector (Int,Double)) IO ()
trainSink params filePath batchSize =
  do label <- liftIO $ readLabelFile filePath
     featurePtr <- go []
     liftIO $ train params label featurePtr
  where go ptrs =
          do xs <- CL.take batchSize
             if P.length xs > 0
                then do let !norm =
                              P.map (sqrt .
                                     VU.foldl' (\a b -> a + b ^ 2) 0 .
                                     snd . VU.unzip)
                                    xs
                            ys =
                              P.zipWith (\x n -> VU.map (\(i,v) -> (i,v / n)) x) xs norm
                        featurePtr <-
                          liftIO $ MP.mapM (getFeaturePtr . VU.toList) ys
                        go $ ptrs P.++ featurePtr
                else return ptrs

-- liblinear feature node's index starts from 1 !!!!!
concatConduit
  :: [Int] -> Conduit [PVPOutputData] IO (VU.Vector (Int,Double))
concatConduit offset =
  awaitForever
    (yield .
     VU.concat .
     P.zipWith (\i x -> VU.map (\(ind,val) -> (ind + i + 1,val)) x) offset .
     P.map parsePVPOutputData)
  where parsePVPOutputData
          :: PVPOutputData -> VU.Vector (Int,Double)
        parsePVPOutputData (PVP_OUTPUT_NONSPIKING_ACT _ xs) =
          VU.filter (\(i,v) -> v /= 0) $ VU.imap (\i v -> (i,v)) xs
        parsePVPOutputData (PVP_OUTPUT_ACT_SPARSEVALUES _ xs) = xs
        parsePVPOutputData _ = error "Doesn't support this type of pvpfile."

concatPooledConduit
  :: Conduit [VU.Vector (Int,Double)] IO (VU.Vector (Int,Double))
concatPooledConduit = awaitForever (yield . VU.concat)

predictConduit
  :: Conduit (VU.Vector (Int,Double)) IO (Ptr C'feature_node)
predictConduit =
  awaitForever
    (\x ->
       do let !norm = sqrt . VU.foldl' (\a b -> a + b ^ 2) 0 . snd . VU.unzip $ x
              y = VU.map (\(i,v) -> (i,v / norm)) x
          ptr <- liftIO $ (getFeaturePtr . VU.toList) y
          yield ptr)
