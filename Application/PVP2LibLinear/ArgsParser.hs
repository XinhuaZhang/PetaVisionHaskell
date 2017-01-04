module Application.PVP2LibLinear.ArgsParser where

import           Data.Maybe
import           PetaVision.Data.Pooling
import           System.Console.GetOpt
import           Text.Parsec
import           Text.Parsec.String
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
  | PoolingSize Int
  deriving (Show)

data Params = Params
  { pvpFile     :: [[String]]  -- The inner-list is the same pvpFile
                               -- from different batches; the
                               -- outter-list is different pvpFile
  , labelFile   :: [String]    -- A list of label files from different batches
  , c           :: Double
  , numThread   :: Int
  , modelName   :: String
  , findC       :: Bool
  , poolingFlag :: Bool
  , poolingType :: PoolingType
  , batchSize   :: Int
  , poolingSize :: Int
  } deriving (Show)

options :: [OptDescr Flag]
options =
  [ Option
      ['i']
      ["pvpfile"]
      (ReqArg PVPFile "FILE")
      "PVPFile (For concatenation, -i file1_batch1 file1_batch2 ... -i file2_batch1 file2_batch2 ...)"
  , Option ['l'] ["Label"] (ReqArg LabelFile "FILE") "Input either pvp or txt label file"
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
  , Option ['p'] ["poolingFlag"] (NoArg Pool) "Use pooling."
  , Option
      ['P']
      ["poolingType"]
      (ReqArg PoolingType "NAME")
      "Set the poolingType. It is either Max or Avg."
  , Option
      ['b']
      ["batchSize"]
      (ReqArg (BatchSize . readInt) "INT")
      "Set the batchSize."
  , Option
      ['s']
      ["poolingSize"]
      (ReqArg (PoolingSize . readInt) "INT")
      "Set pooling size (Defaule 3)."
  , Option ['m'] ["modelName"] (ReqArg ModelName "FILE") "SVM model name"
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
      { pvpFile = [[]]
      , labelFile = []
      , c = 1.0
      , numThread = 1
      , modelName = "model"
      , findC = False
      , poolingFlag = False
      , poolingType = Avg
      , batchSize = 1
      , poolingSize = 3
      }
    go [] params = params
    go (x:xs) params =
      case x of
        PVPFile str ->
          go
            xs
            (params
             { pvpFile = splitStringbySpace str : pvpFile params
             })
        LabelFile str ->
          go
            xs
            (params
             { labelFile = splitStringbySpace str
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
        Pool ->
          go
            xs
            (params
             { poolingFlag = True
             })
        PoolingType str ->
          go
            xs
            (params
             { poolingType = read str :: PoolingType
             })
        BatchSize x' ->
          go
            xs
            (params
             { batchSize = x'
             })
        PoolingSize n ->
          go
            xs
            (params
             { poolingSize = n
             })

parseArgs :: [String] -> IO Params
parseArgs args = do
  flags <- compilerOpts args
  return $ parseFlag flags

spaceParser :: Parser [String]
spaceParser = do
  spaces
  x <- many (noneOf [' '])
  if null x
    then return []
    else do
      xs <- spaceParser
      return $! x : xs

splitStringbySpace :: String -> [String]
splitStringbySpace xs =
  case parse spaceParser "" xs of
    Left err -> error $ show err
    Right ys -> ys
