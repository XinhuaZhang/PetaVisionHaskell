import           Control.Monad            as M
import           Control.Monad.Parallel   as MP
import           Data.Array.Repa          as R
import           Data.List                as L
import           Data.Maybe
import           PetaVision.Image.ImageIO
import           System.Directory
import           System.Environment
import           System.FilePath
import Data.Vector.Unboxed as VU

{-# INLINE split #-}

split :: String -> (String, String)
split xs =
  let idx = L.elemIndex ' ' xs
  in case idx of
       Nothing -> error $ "split: Cannot find space. " L.++ xs
       Just i ->
         let (a, b) = L.splitAt i xs
         in (a, L.tail b)

{-# INLINE labelImage #-}

labelImage :: ImageRepa -> ImageRepa -> ImageRepa
labelImage (Image _ tImg) (Image _ aImg) =
  Image 8 . computeS . R.traverse2 tImg aImg (\a b -> b) $
  (\ft fa (Z :. _ :. i :. j) ->
     if fa (Z :. (0 :: Int) :. i :. j) > 0
       then 1
       else if (L.any (\k -> ft (Z :. k :. i :. j) > 0) [0 .. nf - 1])
              then 0
              else 255)
  where
    (Z :. nf :. _ :. _) = extent tImg
    
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
  (tFilePath:aFilePath:folerPath:_) <- getArgs
  tFilePathList <- L.lines <$> readFile tFilePath
  aFilePathList <- L.lines <$> readFile aFilePath
  removePathForcibly (folerPath </> "t")
  createDirectoryIfMissing True (folerPath </> "t")
  removePathForcibly (folerPath </> "a")
  createDirectoryIfMissing True (folerPath </> "a")
  -- MP.sequence_ $
  --   L.zipWith
  --     (\i tFilePath -> do
  --        img <- readImageRepa tFilePath True
  --        plotImageRepa (folerPath </> "t" </> (show i L.++ ".png")) img)
  --     [1 ..]
  --     tFilePathList
  -- MP.sequence_ $
  --   L.zipWith
  --     (\i aFilePath -> do
  --        img <- readImageRepa aFilePath False
  --        plotImageRepa (folerPath </> "a" </> (show i L.++ ".png")) .
  --          fmap
  --            (computeS .
  --             R.map
  --               (\x ->
  --                  if x > 0
  --                    then 1
  --                    else 0)) $
  --          img)
  --     [1 ..]
  --     aFilePathList
  MP.sequence_ $
    L.zipWith3
      (\i tFilePath aFilePath -> do
         tImg <- readImageRepa tFilePath True
         aImg <- readImageRepa aFilePath False
         plotImageRepa (folerPath </> "t" </> (show i L.++ ".png")) . cleanImage $
           tImg
         plotImageRepa (folerPath </> "a" </> (show i L.++ ".png")) . cleanImage $
           aImg)
      [1 ..]
      tFilePathList
      aFilePathList
  let aImageList =
        [ (folerPath </> "a" </> (show i L.++ ".png"))
        | i <- [1 .. L.length aFilePathList]
        ]
      tImageList =
        [ (folerPath </> "t" </> (show i L.++ ".png"))
        | i <- [1 .. L.length tFilePathList]
        ]
      
  writeFile (folerPath </> "aImageList.txt") . L.unlines $ aImageList
  writeFile (folerPath </> "tImageList.txt") . L.unlines $ tImageList
