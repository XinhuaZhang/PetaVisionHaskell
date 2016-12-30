{-# LANGUAGE TemplateHaskell #-}
module Application.PVP2LibLinear.ArgsParser where

import           Data.List                  as L
import           Data.Maybe
import           PetaVision.Data.Pooling
import           System.Console.GetOpt
import           System.Environment
import           Text.Read

data Flag
  = PVPFile String
  | LabelFile String
  | Thread Int
  | C Double
  | ModelName String
  | FindC
  | Pool
  | PoolingType String
  | BatchSize Int
  | GPUId [Int]
  | PoolingSize Int
  | GPUPooling
  deriving (Show)

data Params =
  Params {pvpFile        :: [String]
         ,labelFile      :: String
         ,c              :: Double
         ,numThread      :: Int
         ,modelName      :: String
         ,findC          :: Bool
         ,poolingFlag    :: Bool
         ,poolingType    :: PoolingType
         ,batchSize      :: Int
         ,gpuId          :: [Int]
         ,poolingSize    :: Int
         ,gpuPoolingFlag :: Bool}

options :: [OptDescr Flag]
options =
  [Option ['i']
          ["pvpfile"]
          (ReqArg PVPFile "FILE")
          "PVPFile (For concatenation, -i file1 -i file2 ...)"
  ,Option ['l']
          ["label"]
          (ReqArg LabelFile "FILE")
          "Input label file"
  ,Option ['c']
          ["constrainC"]
          (ReqArg (\x -> C $ readDouble x) "Double")
          "Set the liblinear parameter c (Default 1)"
  ,Option ['t']
          ["thread"]
          (ReqArg (\x -> Thread $ readInt x) "INT")
          "Set the number of threads as x (\"+RTS -Nx\" should be added at the end of the command)"
  ,Option ['C']
          ["findC"]
          (NoArg FindC)
          "Find parameter C. You may want to specify the initial c value using -c. The default initial c value is 1. Set it to be -1 to let the problem to find a initial value for c"
  ,Option ['p']
          ["poolingFlag"]
          (NoArg Pool)
          "Use pooling."
  ,Option ['P']
          ["poolingType"]
          (ReqArg PoolingType "NAME")
          "Set the poolingType. It is either Max or Avg."
  ,Option ['b']
          ["batchSize"]
          (ReqArg (\x -> BatchSize $ readInt x) "INT")
          "Set the batchSize."
  ,Option ['d']
          ["gpuId"]
          (ReqArg (\x -> let go []  = []
                             go (y:ys) = if y == ','
                                            then go ys
                                            else [y]:go ys
                         in GPUId $ map readInt $ go x) "[INT]")
          "Set GPU ID"
  ,Option ['g']
          ["gpuPooling"]
          (NoArg GPUPooling)
          "Whether or not using GPU for pooling."
  ,Option ['s']
          ["poolingSize"]
          (ReqArg (\x -> PoolingSize $ readInt x) "INT")
          "Set pooling size (Defaule 3)."]
  ,Option ['m']
          ["modelName"]
          (ReqArg ModelName "FILE")
          "model name"


readInt :: String -> Int
readInt str =
  case (readMaybe str :: Maybe Int) of
    Nothing -> error $ "\nRead integer error: " ++ str
    Just x  -> x

readDouble :: String -> Double
readDouble str =
  case (readMaybe str :: Maybe Double) of
    Nothing -> error $ "\nRead double error: " ++ str
    Just x  -> x

compilerOpts :: [String] -> IO [Flag]
compilerOpts argv =
  case getOpt Permute options argv of
    (o,[],[])      -> return o
    (_,nonOpts,[]) -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_,_,errs)     -> error (concat errs ++ usageInfo header options)
  where header = "Usage: ic [OPTION...] files..."


parseFlag :: [Flag] -> Params
parseFlag flags = go flags defaultFlag
  where defaultFlag =
          Params {pvpFile = []
                 ,labelFile = ""
                 ,c = 1.0
                 ,numThread = 1
                 ,modelName = "model"
                 ,findC = False
                 ,poolingFlag = False
                 ,poolingType = Avg
                 ,batchSize = 1
                 ,gpuId = [0]
                 ,gpuPoolingFlag = False
                 ,poolingSize = 3}
        go [] params = params
        go (x:xs) params =
          case x of
            PVPFile str -> go xs (params {pvpFile = str : (pvpFile params)})
            LabelFile str -> go xs (params {labelFile = str})
            Thread n -> go xs (params {numThread = n})
            C v -> go xs (params {c = v})
            ModelName str -> go xs (params {modelName = str})
            FindC -> go xs (params {findC = True})
            Pool -> go xs (params {poolingFlag = True})
            PoolingType str ->
              go xs (params {poolingType = read str :: PoolingType})
            BatchSize x -> go xs (params {batchSize = x})
            GPUId x -> go xs (params {gpuId = x})
            GPUPooling -> go xs (params {gpuPoolingFlag = True})
            PoolingSize n -> go xs (params {poolingSize = n})

parseArgs :: [String] -> IO Params
parseArgs args =
  do flags <- compilerOpts args
     return $ parseFlag flags
