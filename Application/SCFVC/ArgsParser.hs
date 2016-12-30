module Application.SCFVC.ArgsParser where

import           Data.Maybe
import           System.Console.GetOpt
import           Text.Read

data Flag
  = ActFile String
  | ErrorFile String
  | LabelFile String
  | Thread Int
  | C Double
  | ModelName String
  | FindC
  | BatchSize Int
  | Pool
  deriving (Show)

data Params = Params
  { actFile     :: [String]
  , errorFile   :: [String]
  , labelFile   :: String
  , c           :: Double
  , numThread   :: Int
  , modelName   :: String
  , findC       :: Bool
  , poolingFlag :: Bool
  , batchSize   :: Int
  } deriving (Show)

options :: [OptDescr Flag]
options =
  [ Option
      ['i']
      ["actfile"]
      (ReqArg ActFile "FILE")
      "ActFile (For concatenation, -i file1 -i file2 ...)"
  , Option
      ['i']
      ["errfile"]
      (ReqArg ErrorFile "FILE")
      "ErrorFile (For concatenation, -i file1 -i file2 ...)"
  , Option ['l'] ["label"] (ReqArg LabelFile "FILE") "Input label file"
  , Option
      ['c']
      ["constrainC"]
      (ReqArg (C . readDouble) "Double")
      "Set the liblinear parameter c (Default 1)"
  , Option
      ['t']
      ["thread"]
      (ReqArg (Thread . readInt) "INT")
      "Set the number of threads as x (\"+RTS -Nx\" should be added at the end of the command)"
  , Option
      ['C']
      ["findC"]
      (NoArg FindC)
      "Find parameter C. You may want to specify the initial c value using -c. The default initial c value is 1. Set it to be -1 to let the problem to find a initial value for c"
  , Option
      ['b']
      ["batchSize"]
      (ReqArg (BatchSize . readInt) "INT")
      "Set the batchSize."
  , Option ['p'] ["poolingFlag"] (NoArg Pool) "Use pooling."
  ]

readInt :: String -> Int
readInt str =
  fromMaybe
    (error $ "\nRead integer error: " ++ str)
    (readMaybe str :: Maybe Int)

readDouble :: String -> Double
readDouble str =
  fromMaybe
    (error $ "\nRead double error: " ++ str)
    (readMaybe str :: Maybe Double)

compilerOpts :: [String] -> IO [Flag]
compilerOpts argv =
  case getOpt Permute options argv of
    (o, [], [])      -> return o
    (_, nonOpts, []) -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_, _, errs)     -> error (concat errs ++ usageInfo header options)
  where
    header = "Usage: ic [OPTION...] files..."

parseFlag :: [Flag] -> Params
parseFlag flags = go flags defaultFlag
  where
    defaultFlag =
      Params
      { actFile = []
      , errorFile = []
      , labelFile = ""
      , c = 1.0
      , numThread = 1
      , modelName = "model"
      , findC = False
      , batchSize = 1
      , poolingFlag = False
      }
    go [] params = params
    go (x:xs) params =
      case x of
        ActFile str ->
          go
            xs
            (params
             { actFile = str : actFile params
             })
        ErrorFile str ->
          go
            xs
            (params
             { errorFile = str : errorFile params
             })
        LabelFile str ->
          go
            xs
            (params
             { labelFile = str
             })
        Thread n ->
          go
            xs
            (params
             { numThread = n
             })
        C v ->
          go
            xs
            (params
             { c = v
             })
        ModelName str ->
          go
            xs
            (params
             { modelName = str
             })
        FindC ->
          go
            xs
            (params
             { findC = True
             })
        BatchSize x' ->
          go
            xs
            (params
             { batchSize = x'
             })
        Pool ->
          go
            xs
            (params
             { poolingFlag = True
             })

parseArgs :: [String] -> IO Params
parseArgs args = do
  flags <- compilerOpts args
  return $ parseFlag flags
