{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternGuards              #-}
module Classifier.LibLinear.Interface where

import           Classifier.LibLinear.Bindings
import           Classifier.LibLinear.Example
import           Classifier.LibLinear.Solver
import           Control.Monad                 as M
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List             as CL
import           Data.List                     as L
import           Data.Vector.Unboxed           as VU
import           Foreign                       as F
import           Foreign.C
import           Foreign.Marshal.Array
import           Prelude                       as P
import           System.IO
import           Text.Printf


data TrainParams = TrainParams
  { trainSolver          :: Solver
  , trainC               :: Double
  , trainNumExamples     :: Int
  , trainFeatureIndexMax :: Int
  , trainModel           :: String
  } deriving (Show)

train
  :: TrainParams -> [Double] -> [Ptr C'feature_node] -> IO ()
train (TrainParams solver c numExample maxIndex modelName) label feature =
  do labelPtr <- getLabelVecPtr label
     featurePtr <- newArray feature
     let p =
           C'problem {c'problem'l = fromIntegral numExample
                     ,c'problem'n = fromIntegral maxIndex
                     ,c'problem'y = labelPtr
                     ,c'problem'x = featurePtr
                     ,c'problem'bias = -1.0}
     model <-
       with p
            (\problem' ->
               with (newParameter solver c)
                    (\param' -> c'train problem' param'))
     modelName <- newCString modelName
     c'save_model modelName model


predict
  :: String -> FilePath -> Sink (Double,Ptr C'feature_node) (ResourceT IO) ()
predict predictModel output = do
  modelName <- liftIO $ newCString predictModel
  model <- liftIO $ c'load_model modelName
  h <- liftIO $ openFile output WriteMode
  (correct, total) <- func h model (0, 0)
  let percent = fromIntegral correct / (fromIntegral total :: Double) * 100
  liftIO $ printf "%f%%\n" percent
  liftIO $ hClose h
  where
    func
      :: Handle
      -> Ptr C'model
      -> (Int, Int)
      -> Sink (Double, Ptr C'feature_node) (ResourceT IO) (Int, Int)
    func handle model (!correct, !total) = do
      x <- await
      case x of
        Just (!target, !xs) -> do
          prediction <- liftIO $ c'predict model xs
          liftIO . hPutStr handle $
            printf "(%d,%d)\n" (round target :: Int) (round prediction :: Int)
          if (round target :: Int) == round prediction
            then func handle model (correct + 1, total + 1)
            else func handle model (correct, total + 1)
        Nothing -> return (correct, total)

findParameterC
  :: TrainParams -> [Double] -> [Ptr C'feature_node] -> IO ()
findParameterC (TrainParams solver c numExample maxIndex modelName) label feature =
  do labelPtr <- getLabelVecPtr label
     featurePtr <- newArray feature
     let p =
           C'problem {c'problem'l = fromIntegral numExample
                     ,c'problem'n = fromIntegral maxIndex
                     ,c'problem'y = labelPtr
                     ,c'problem'x = featurePtr
                     ,c'problem'bias = -1.0}
     with p
          (\problem' ->
             with (newParameter solver c)
                  (\param' ->
                     allocaBytes
                       8
                       (\bestC' ->
                          allocaBytes
                            8
                            (\bestRate' ->
                               do c'find_parameter_C problem'
                                                     param'
                                                     5
                                                     (realToFrac c)
                                                     1024
                                                     bestC'
                                                     bestRate'
                                  bestC <- F.peek bestC'
                                  bestRate <- F.peek bestRate'
                                  putStrLn $
                                    "Best C = " L.++
                                    show bestC L.++
                                    " CV accuracy = " L.++
                                    show (100 * bestRate) L.++ "%"))))

predictVoting
  :: String
  -> Sink (Double, Ptr C'feature_node) (ResourceT IO) (VU.Vector Int, [((Int, Int), [Double])])
predictVoting predictModel = do
  modelName <- liftIO $ newCString predictModel
  model <- liftIO $ c'load_model modelName
  (CInt numClass32) <- liftIO $ c'get_nr_class model
  let numClass = fromIntegral numClass32
  probPtr <- liftIO $ callocArray numClass
  labels <-
    liftIO $
    allocaArray numClass $ \p -> do
      c'get_labels model p
      (VU.fromList . L.map fromIntegral) <$> peekArray numClass p
  ys <- func model probPtr numClass []
  return (labels, ys)
  where
    func
      :: Ptr C'model
      -> Ptr CDouble
      -> Int
      -> [((Int, Int), [Double])]
      -> Sink (Double, Ptr C'feature_node) (ResourceT IO) [((Int, Int), [Double])]
    func model probPtr numClass ys = do
      x <- await
      case x of
        Just (!target, !xs) -> do
          -- prediction <- liftIO $ c'predict_probability model xs probPtrb
          prediction <- liftIO $ c'predict model xs 
          prob <- liftIO $ peekArray numClass probPtr
          let !y =
                ( (round target, round prediction)
                , L.map (\(CDouble z) -> z) prob)
          func model probPtr numClass $! (y : ys)
        Nothing -> return . L.reverse $ ys
