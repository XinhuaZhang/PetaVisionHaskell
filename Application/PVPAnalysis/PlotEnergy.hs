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

data EnergyInfo =
  EnergyInfo !Int
             !Int
             !Double
  deriving (Show)

{-# INLINE parseEnergyInfo #-}

parseEnergyInfo :: Text -> EnergyInfo
parseEnergyInfo txt =
  case (double . TL.dropWhile (not . isDigit) $ txt) of
    Left msg1 -> error msg1
    Right (time, txt1) ->
      case (decimal . TL.dropWhile (not . isDigit) $ txt1) of
        Left msg2 -> error msg2
        Right (batchNum, txt2) ->
          case (double . TL.dropWhile (not . isDigit) $ txt2) of
            Left msg3       -> error msg3
            Right (e, txt2) -> EnergyInfo (round time) batchNum e

energyFileSource :: FilePath -> Source IO EnergyInfo
energyFileSource filePath = do
  h <- liftIO $ openFile filePath ReadMode
  x <- liftIO $ TL.hGetLine h
  xs <- liftIO $ TL.hGetContents h
  sourceList . L.map parseEnergyInfo . TL.lines $ xs
  liftIO $ hClose h

sink :: Int -> Double -> Sink EnergyInfo IO [(Int,Double)]
sink n m = do
  CL.drop n
  xs <- CL.consume
  return $!
    L.map
      (\(EnergyInfo t _ e) ->
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
  (folderPath:numBatchStr:numLastTakeStr:displayPeriodStr:thresholdStr:name:_) <-
    getArgs
  let -- str = "TopLayerEnergyProbe_batchElement_"
      str = name L.++ "EnergyProbe_batchElement_"
      fileNames =
        L.map
          (\i -> folderPath L.++ "/" L.++ str L.++ show i L.++ ".txt")
          [0 .. (read numBatchStr :: Int) - 1]
  print $ L.head fileNames
  numLines <- fmap (L.length . L.lines) . IO.readFile $ (L.head fileNames)
  print numLines
  xs <-
    M.mapM
      (\path ->
         energyFileSource path $$
         sink
           (numLines - (read numLastTakeStr :: Int))
           (read thresholdStr :: Double))
      fileNames
  print . L.length . takeEven (read displayPeriodStr :: Int) . L.head $ xs
  toFile def (name L.++ "energy.png") $ do
    layout_title .= name L.++ "Energy"
    M.zipWithM_
      -- (\i x -> plot (line (show i) [takeEven (read displayPeriodStr :: Int) x]))
      (\i x -> plot (line (show i) [x]))
      [0 ..]
      xs
