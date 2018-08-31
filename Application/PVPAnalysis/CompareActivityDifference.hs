import           Application.PVPAnalysis.Analysis
import           Control.Monad                          as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List                      as CL
import           Data.List                              as L
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           PetaVision.PVPFile.IO
import           System.Environment
import           System.IO

takeOdd :: [a] -> [a]
takeOdd [] = []
takeOdd (x:[]) = [x]
takeOdd (x:y:xs) = x : takeOdd xs

main = do
  (filePath:numActStr:_) <- getArgs
  xs <- runResourceT $ pvpFileSource filePath $$ CL.consume
  let ys = takeOdd . L.drop (L.length xs - (read numActStr :: Int)) $ xs
  M.zipWithM_
    (\y i ->
       toFile def ("xor_" L.++ show i L.++ ".png") $ do
         layout_title .= "XOR Difference"
         plot
           (line
              ""
              [ L.zipWith
                  (\j x -> (j :: Int, diffXORPVPOutputData y x))
                  [1 ..]
                  ys
              ]))
    ys
    [1 ..]
  M.zipWithM_
    (\y i ->
       toFile def ("abs_" L.++ show i L.++ ".png") $ do
         layout_title .= "ABS Difference"
         plot
           (line
              ""
              [ L.zipWith
                  (\j x -> (j :: Int, diffACTPVPOutputData y x))
                  [1 ..]
                  ys
              ]))
    ys
    [1 ..]
