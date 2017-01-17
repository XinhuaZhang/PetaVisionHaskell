{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternGuards              #-}
module Classifier.LibLinear.Interface where

import           Classifier.LibLinear.Bindings
import           Classifier.LibLinear.Example
import           Classifier.LibLinear.Solver
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List             as CL
import           Foreign                       as F
import           Foreign.C
import           Foreign.Marshal.Array
import           Prelude                       as P
import           System.IO


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
  (correct, total) <- func model (0, 0)
  let percent = fromIntegral correct / (fromIntegral total :: Double) * 100
      str = show percent
  liftIO $ putStrLn str
  h <- liftIO $ openFile output WriteMode
  liftIO $ hPutStrLn h str
  liftIO $ hClose h
  where
    func
      :: Ptr C'model
      -> (Int, Int)
      -> Sink (Double, Ptr C'feature_node) (ResourceT IO) (Int, Int)
    func model (!correct, !total) = do
      x <- await
      case x of
        Just (!target, !xs) -> do
          prediction <- liftIO $ c'predict model xs
          let !correctNew =
                if (round target :: Int) == round prediction
                  then correct + 1
                  else correct
          if (round target :: Int) == round prediction
            then func model (correct + 1, total + 1)
            else func model (correct, total + 1)
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
                                    "Best C = " ++
                                    show bestC ++
                                    " CV accuracy = " ++
                                    show (100 * bestRate) ++ "%"))))
