import           Control.Monad            as M
import           Control.Monad.Parallel   as MP
import           Data.Array.Repa          as R
import           Data.List                as L
import           Data.Maybe
import           Data.Vector.Unboxed      as VU
import           PetaVision.Image.ImageIO
import           System.Directory
import           System.Environment
import           System.FilePath

{-# INLINE cleanImage #-}

cleanImage :: ImageRepa -> ImageRepa
cleanImage (Image _ img) =
  let (Z :. nf :. rows :. cols) = extent img
      xs =
        L.map
          (\i -> toUnboxed . computeS . R.slice img $ (Z :. i :. All :. All))
          [0 .. nf - 1]
      y =
        VU.map
          (\x ->
             if x > 0
               then 255
               else 0) .
        L.foldl1' (VU.zipWith (+)) $
        xs
  in Image 8 . fromUnboxed (Z :. (1 :: Int) :. rows :. cols) $ y

main = do
  (fileListPath:labelListPath:writeFolderPath:_) <- getArgs
  createDirectoryIfMissing True writeFolderPath
  fileList <- L.lines <$> readFile fileListPath
  copyFile labelListPath (writeFolderPath </> (takeFileName labelListPath))
  MP.sequence_ $
    L.zipWith
      (\i filePath -> do
         img <- readImageRepa filePath False
         plotImageRepa (writeFolderPath </> (show i L.++ ".png")) . cleanImage $
           img)
      [1 ..]
      fileList
  writeFile (writeFolderPath </> "imageList.txt") . L.unlines . L.map (\i -> writeFolderPath </> show i L.++ ".png") $
    [1 .. L.length fileList]
