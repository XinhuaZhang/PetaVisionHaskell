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
        
getGridImage :: PVPWeight -> DynamicImage
getGridImage weight =
  case nfp' of
    3 ->
      ImageRGB8 $
      generateImage
        (\i j ->
           let imgRowIdx = div i gridImgHeight
               imgColIdx = div j gridImgWidth
               pixelRowIdx = mod i gridImgHeight
               pixelColIdx = mod j gridImgWidth
               pixelRowImgIdx = pixelRowIdx - numPixelBorderY
               pixelColImgIdx = pixelColIdx - numPixelBorderX
           in if (pixelRowImgIdx < 0) ||
                 (pixelColImgIdx < 0) ||
                 (pixelRowImgIdx >= nyp') || (pixelColImgIdx >= nxp')
                 then maxValRGB
                 else let n = i * nc + j
                          r =
                            fromIntegral . round $
                            weight !
                            (Z :. pixelRowImgIdx :. pixelColImgIdx :. 0 :. n)
                          g =
                            fromIntegral . round $
                            weight !
                            (Z :. pixelRowImgIdx :. pixelColImgIdx :. 1 :. n)
                          b =
                            fromIntegral . round $
                            weight !
                            (Z :. pixelRowImgIdx :. pixelColImgIdx :. 2 :. n)
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
               pixelRowImgIdx = pixelRowIdx - numPixelBorderY
               pixelColImgIdx = pixelColIdx - numPixelBorderX
           in if (pixelRowImgIdx < 0) ||
                 (pixelColImgIdx < 0) ||
                 (pixelRowImgIdx >= nyp') || (pixelColImgIdx >= nxp')
                 then maxValY
                 else let n = i * nc + j
                      in if n > (numPatches' - 1)
                            then maxValY
                            else fromIntegral . round $
                                 weight !
                                 (Z :. pixelRowImgIdx :. pixelColImgIdx :. 0 :. n))
        numPixelCols
        numPixelRows
  where maxValRGB =
          PixelRGB8 (maxBound :: Pixel8)
                    (maxBound :: Pixel8)
                    (maxBound :: Pixel8)
        maxValY = maxBound :: Pixel8
        (Z :. nyp' :. nxp' :. nfp' :. numPatches') = extent weight
        numPixelBorderX = round (fromIntegral nxp' / 4)
        numPixelBorderY = round (fromIntegral nyp' / 4)
        (nr,nc) = getGridRowsCols weight
        gridImgHeight = nyp' + 2 * numPixelBorderY
        numPixelRows = nr * gridImgHeight
        gridImgWidth = nxp' + 2 * numPixelBorderX
        numPixelCols = nc * gridImgWidth
