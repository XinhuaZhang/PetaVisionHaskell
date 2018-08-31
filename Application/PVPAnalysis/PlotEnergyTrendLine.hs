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
import           System.FilePath
import           System.IO
import           Text.Printf

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

conduit :: Int -> Conduit EnergyInfo IO (Int, Double)
conduit n = do
  CL.drop n
  x <- await
  case x of
    Nothing -> return ()
    Just y -> do
      yield . (\(EnergyInfo t _ e) -> (t, e)) $ y
      conduit n

main = do
  (folderPath1:folderPath2:numBatchStr:displayPeriodStr:numStr:_) <-
    getArgs
  let str = "EnergyProbe_batchElement_"
      fileNames1 =
        L.map
          (\i -> folderPath1 L.++ "/" L.++ str L.++ show i L.++ ".txt")
          [0 .. (read numBatchStr :: Int) - 1]
      fileNames2 =
        L.map
          (\i -> folderPath2 L.++ "/" L.++ str L.++ show i L.++ ".txt")
          [0 .. (read numBatchStr :: Int) - 1]
  xs1 <-
    M.mapM
      (\path ->
         energyFileSource path $$ conduit ((read displayPeriodStr :: Int) - 1) =$=
         CL.take (read numStr :: Int))
      fileNames1
  xs2 <-
    M.mapM
      (\path ->
         energyFileSource path $$ conduit ((read displayPeriodStr :: Int) - 1) =$=
         CL.take (read numStr :: Int))
      fileNames2
  toFile def "energy.png" $ do
    layout_title .= "Energy"
    plot (line (takeBaseName folderPath1) [(L.zip [1 :: Double ..] . snd . L.unzip . L.concat $ xs1)])
    plot (line (takeBaseName folderPath2) [(L.zip [1 :: Double ..] . snd . L.unzip . L.concat $ xs2)])
