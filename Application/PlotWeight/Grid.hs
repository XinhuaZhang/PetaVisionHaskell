module Application.PlotWeight.Grid where


import           Codec.Picture
import           Data.Array.Repa        as R
import           PetaVision.Data.Weight
import           Prelude                as P


getGridRowsCols :: PVPWeight -> (Int,Int)
getGridRowsCols weight = (r,c)
  where (Z :. _ :. _ :. _ :. n) = extent weight
        c = round . sqrt . fromIntegral $ n
        r = ceiling $ (fromIntegral n) / (fromIntegral c)
        
getGridImage :: PVPWeight -> Int -> DynamicImage
getGridImage weight numPixelBorder =
  case nfp' of
    3 ->
      ImageRGB8 $
      generateImage
        (\i j ->
           let imgRowIdx = div i gridImgHeight
               imgColIdx = div j gridImgWidth
               pixelRowIdx = mod i gridImgHeight
               pixelColIdx = mod j gridImgWidth
           in if (pixelRowIdx - numPixelBorder < 0) ||
                 (pixelColIdx - numPixelBorder < 0) ||
                 (pixelRowIdx - numPixelBorder >= nyp') ||
                 (pixelColIdx - numPixelBorder >= nxp')
                 then maxValRGB
                 else let n = i * nc + j
                          r =
                            fromIntegral . round $
                            weight !
                            (Z :. pixelRowIdx :. pixelColIdx :. 0 :. n)
                          g =
                            fromIntegral . round $
                            weight !
                            (Z :. pixelRowIdx :. pixelColIdx :. 1 :. n)
                          b =
                            fromIntegral . round $
                            weight !
                            (Z :. pixelRowIdx :. pixelColIdx :. 2 :. n)
                      in if n > (numPatches' - 1)
                            then maxValRGB
                            else PixelRGB8 r g b)
        numPixelCols
        numPixelRows
    1 ->
      ImageY8 $
      generateImage
        (\i j ->
           let imgRowIdx = div i gridImgHeight
               imgColIdx = div j gridImgWidth
               pixelRowIdx = mod i gridImgHeight
               pixelColIdx = mod j gridImgWidth
           in if (pixelRowIdx - numPixelBorder < 0) ||
                 (pixelColIdx - numPixelBorder < 0) ||
                 (pixelRowIdx - numPixelBorder >= nyp') ||
                 (pixelColIdx - numPixelBorder >= nxp')
                 then maxValY
                 else let n = i * nc + j
                      in if n > (numPatches' - 1)
                            then maxValY
                            else fromIntegral . round $
                                 weight !
                                 (Z :. pixelRowIdx :. pixelColIdx :. 0 :. n))
        numPixelCols
        numPixelRows
  where maxValRGB =
          PixelRGB8 (maxBound :: Pixel8)
                    (maxBound :: Pixel8)
                    (maxBound :: Pixel8)
        maxValY = maxBound :: Pixel8
        (Z :. nyp' :. nxp' :. nfp' :. numPatches') = extent weight
        (nr,nc) = getGridRowsCols weight
        gridImgHeight = nyp' + 2 * numPixelBorder
        numPixelRows = nr * gridImgHeight
        gridImgWidth = nxp' + 2 * numPixelBorder
        numPixelCols = nc * gridImgWidth
