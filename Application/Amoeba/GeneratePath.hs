import           Control.Monad      as M
import           Data.List          as L
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Random
import           Text.Printf

{-# INLINE shuffle #-}

shuffle :: [a] -> IO [a]
shuffle xs = do
  ys <- M.replicateM (L.length xs) randomIO :: IO [Double]
  return . fst . L.unzip . L.sortOn snd $ L.zip xs ys

{-# INLINE pathGenerator #-}

pathGenerator :: FilePath -> Int -> String -> Int -> FilePath
pathGenerator folderPath freq "distractor" n =
  printf "%s/%dFC/distractor/amoeba_0_%d_%05d.png" folderPath freq freq n
pathGenerator folderPath freq "target" n =
  printf "%s/%dFC/target/amoeba_1_%d_%05d.png" folderPath freq freq n
pathGenerator folderPath freq "amoeba" n =
  printf "%s/%dFC/amoeba/amoeba_2_%d_%05d.png" folderPath freq freq n
pathGenerator _ _ x _ = error $ printf "pathGenerator: type %s not found.\n" x

main = do
  (folderPath:writeFolderPath:nullImagePath:totalNumStr:numTrainStr:xs) <-
    getArgs
  let fcs = L.map (\x -> read x :: Int) xs
      totalNum = read totalNumStr :: Int
      numTrain = read numTrainStr :: Int
  numFile <-
    L.length <$>
    listDirectory (folderPath </> (L.head xs L.++ "FC") </> "target")
  unless
    (totalNum == numFile)
    (error $
     printf
       "TotalNum:%d\n%d files in %s\n"
       totalNum
       numFile
       (folderPath </> (L.head xs L.++ "FC") </> "target"))
  when
    (numTrain >= totalNum)
    (error $ printf "numTrain: %d\ntotalNum: %d\n" numTrain totalNum)
  createDirectoryIfMissing True (writeFolderPath </> numTrainStr)
  let trainIndex = [1 .. numTrain]
      testIndex = [(numTrain + 1) .. totalNum]
  -- Train
  let amoebaPathList =
        L.concatMap
          (\fc ->
             L.map
               (\n ->
                  ( 1
                  , pathGenerator folderPath fc "target" n
                  , pathGenerator folderPath fc "amoeba" n))
               trainIndex)
          fcs
      distractorPathList =
        L.concatMap
          (\fc ->
             L.map
               (\n ->
                  (0, pathGenerator folderPath fc "distractor" n, nullImagePath))
               trainIndex)
          fcs
  shuffledPairs <- shuffle $ amoebaPathList L.++ distractorPathList
  writeFile (writeFolderPath </> numTrainStr </> "trainImageListAmoebaOnly.txt") .
    L.unlines . L.map (\(_, x, _) -> x) $
    amoebaPathList
  writeFile
    (writeFolderPath </> numTrainStr </> "trainAmoebaListAmoebaOnly.txt") .
    L.unlines . L.map (\(_, _, x) -> x) $
    amoebaPathList
  writeFile (writeFolderPath </> numTrainStr </> "trainImageList.txt") .
    L.unlines . L.map (\(_, x, _) -> x) $
    shuffledPairs
  writeFile (writeFolderPath </> numTrainStr </> "trainAmoebaList.txt") .
    L.unlines . L.map (\(_, _, x) -> x) $
    shuffledPairs
  writeFile (writeFolderPath </> numTrainStr </> "trainLabelList.txt") .
    L.unlines . L.map (\(x, _, _) -> show x) $
    shuffledPairs
  -- Test
  let amoebaPathList =
        L.concatMap
          (\fc ->
             L.map
               (\n ->
                  ( 1
                  , pathGenerator folderPath fc "target" n
                  , pathGenerator folderPath fc "amoeba" n))
               testIndex)
          fcs
      distractorPathList =
        L.concatMap
          (\fc ->
             L.map
               (\n ->
                  (0, pathGenerator folderPath fc "distractor" n, nullImagePath))
               testIndex)
          fcs
  shuffledPairs <- shuffle $ amoebaPathList L.++ distractorPathList
  writeFile (writeFolderPath </> numTrainStr </> "testImageList.txt") .
    L.unlines . L.map (\(_, x, _) -> x) $
    shuffledPairs
  writeFile (writeFolderPath </> numTrainStr </> "testAmoebaList.txt") .
    L.unlines . L.map (\(_, _, x) -> x) $
    shuffledPairs
  writeFile (writeFolderPath </> numTrainStr </> "testLabelList.txt") .
    L.unlines . L.map (\(x, _, _) -> show x) $
    shuffledPairs
