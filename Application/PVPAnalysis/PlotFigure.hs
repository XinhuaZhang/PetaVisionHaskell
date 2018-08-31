module Application.PVPAnalysis.PlotFigure where

import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import           Data.Default.Class
import           Data.List                              as L
import           Data.Vector.Unboxed                    as VU
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo
import           Text.Printf

plotSparsityNHistogram
  :: FilePath -> (Double,VU.Vector Double) -> IO (PickFn ())
plotSparsityNHistogram filePath (percent,vec) =
  renderableToFile def filePath $ toRenderable layout
  where nf' = VU.length vec
        xs =
          L.zipWith (\i v -> (i,[v]))
                    [1 ..] .
          L.reverse .
          L.sort .
          VU.toList 
          -- VU.map (\x -> fromIntegral x / fromIntegral (VU.sum vec) :: Double) 
          $
          vec
        title = printf "Sparsity: %0.1f%%(%d/%d Avg. Activatived)\n"
                       (percent * 100 :: Double)
                       (round $ percent * fromIntegral nf' :: Int)
                       nf'
        layout = layout_title .~ title
               $ layout_title_style . font_size .~ 20
               $ layout_left_axis_visibility . axis_show_ticks .~ False
               $ layout_plots .~ [ plotBars bars ]
               $ def :: Layout PlotIndex Double
        bars = plot_bars_titles .~ [""]
             $ plot_bars_values .~ xs
             $ plot_bars_style .~ BarsClustered
             $ plot_bars_spacing .~ BarsFixGap 0 5
             $ plot_bars_item_styles .~ L.map mkStyle (cycle defaultColorSeq)
             $ def
        mkStyle c = (solidFillStyle c, Just (solidLine 1.0 $ opaque black))


plotHistogram :: FilePath -> VU.Vector Double -> IO (PickFn ())
plotHistogram filePath vec = renderableToFile def filePath $ toRenderable layout
  where
    nf' = VU.length vec
    xs = L.zipWith (\i v -> (i, [v])) [1 ..] . VU.toList $ vec
    title = ""
    layout =
      layout_title .~ title $ layout_title_style . font_size .~ 20 $
      layout_left_axis_visibility .
      axis_show_ticks .~
      False $
      layout_plots .~
      [plotBars bars] $
      def :: Layout PlotIndex Double
    bars =
      plot_bars_titles .~ [""] $ plot_bars_values .~ xs $ plot_bars_style .~
      BarsClustered $
      plot_bars_spacing .~
      BarsFixGap 0 5 $
      plot_bars_item_styles .~
      L.map mkStyle (cycle defaultColorSeq) $
      def
    mkStyle c = (solidFillStyle c, Just (solidLine 1.0 $ opaque black))


plotActivation :: FilePath -> String  -> VU.Vector Double -> IO (PickFn ())
plotActivation filePath title vec =
  renderableToFile def filePath $ toRenderable layout
  where
    nf' = VU.length vec
    xs = [L.zipWith (\i v -> (i, v)) [1 :: Double ..] . VU.toList $ vec]
    layout =
      layout_title .~ title $ layout_title_style . font_size .~ 20 $
      layout_left_axis_visibility .
      axis_show_ticks .~
      False $
      layout_plots .~
      [toPlot lines] $
      def :: Layout Double Double
    lines =
      plot_lines_title .~ "" $ plot_lines_style .~ defaultPlotLineStyle $
      plot_lines_values .~
      xs $
      plot_lines_limit_values .~
        -- (LMin, LValue . VU.minimum $ vec), (LMax, LValue . VU.maximum $ vec)
      [[]] $
      def :: PlotLines Double Double
