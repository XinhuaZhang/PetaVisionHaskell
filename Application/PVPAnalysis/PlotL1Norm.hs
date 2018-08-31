import           Control.Monad                          as M
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Conduit
import           Data.Conduit.List                      as CL
import           Data.List                              as L
import           Data.Text.Lazy                         as TL
import           Data.Text.Lazy.IO                      as TL
import           Data.Text.Lazy.Read                    as TL
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           System.Environment
import           System.IO as IO

data L1NormInfo =
  L1NormInfo !Int
             !Int
             !Int
             !Double
  deriving (Show)

{-# INLINE parseL1NormInfo #-}

parseL1NormInfo :: Text -> L1NormInfo
parseL1NormInfo txt =
  case (double . TL.dropWhile (not . isDigit) $ txt) of
    Left msg1 -> error msg1
    Right (time, txt1) ->
      case (decimal . TL.dropWhile (not . isDigit) $ txt1) of
        Left msg2 -> error msg2
        Right (batchNum, txt2) ->
          case (decimal . TL.dropWhile (not . isDigit) $ txt2) of
            Left msg3 -> error msg3
            Right (numNeurons, txt3) ->
              case (double .
                    TL.dropWhile (not . isDigit) .
                    TL.drop 1 . TL.dropWhile (not . isDigit) $
                    txt3) of
                Left msg4 -> error msg4
                Right (l1, txt4) ->
                  L1NormInfo (round time) batchNum numNeurons l1

energyFileSource :: FilePath -> Source IO L1NormInfo
energyFileSource filePath = do
  h <- liftIO $ openFile filePath ReadMode
  x <- liftIO $ TL.hGetLine h
  xs <- liftIO $ TL.hGetContents h
  sourceList . L.map parseL1NormInfo . TL.lines $ xs
  liftIO $ hClose h

sink :: Int -> Double -> Sink L1NormInfo IO [(Int,Double)]
sink n m = do
  CL.drop n
  xs <- CL.consume
  return $!
    L.map
      (\(L1NormInfo t _ _ e) ->
         if e > m
           then (t, m)
           else (t, e))
      xs
      
takeOdd :: Int -> [a] -> [a]
takeOdd 0 xs = xs
takeOdd _ [] = []
takeOdd n xs = (L.take n xs) L.++ (takeOdd n . L.drop (2 * n) $ xs)


takeEven :: Int -> [a] -> [a]
takeEven 0 xs = xs
takeEven _ [] = []
takeEven n xs =
  (L.drop n . L.take (2 * n) $ xs) L.++ (takeEven n . L.drop (2 * n) $ xs)


main = do
  (folderPath:numBatchStr:numLastTakeStr:displayPeriodStr:thresholdStr:_) <- getArgs
  let str = "S3_L1ProbeTop" 
      fileNames =
        L.map
          (\i -> folderPath L.++ "/" L.++ str 
            L.++ ".txt")
          [0 .. (read numBatchStr :: Int) - 1]
  print (L.head fileNames)
  numLines <- fmap (L.length . L.lines) . IO.readFile $ (L.head fileNames)
  print numLines
  xs <-
    M.mapM
      (\path -> energyFileSource path $$ sink (numLines - (read numLastTakeStr :: Int)) (read thresholdStr :: Double)) 
      fileNames
  print . L.length . takeEven (read displayPeriodStr :: Int) . L.head $ xs
  toFile def "l1norm.png" $ do
    layout_title .= "L1Norm"
    M.zipWithM_ (\i x -> plot (line (show i) [takeOdd (read displayPeriodStr :: Int) x])) [0 ..] xs
