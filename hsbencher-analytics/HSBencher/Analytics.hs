

module HSBencher.Analytics where



import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Default.Class
import Control.Lens


import HSBencher.Internal.Fusion

import Data.List hiding (init)
import Prelude hiding (init) 

---------------------------------------------------------------------------
--

pullEntireTable cid sec table_name = do
  (table_id,auth) <- init cid sec table_name
  getSomething auth table_id "*"
  







---------------------------------------------------------------------------
--
type Line  = [(Double,Double)] 
type Lines = [(String, Line)] 


plotLines ls title fn = renderableToFile def (linePlot title ls) fn

linePlot :: String -> Lines -> Renderable (LayoutPick Double Double Double)
linePlot plot_title ls = layoutToRenderable $ chartIT ls  
  where 
    chartIT :: Lines -> Layout Double Double
    chartIT ls = layout
      
    lineStyle c = line_width .~ 3 
                  $ line_color .~ c
                  $ def ^. plot_lines_style
    layout =
      layout_title .~ plot_title 
      $ layout_title_style . font_size .~ 10
      $ layout_left_axis_visibility . axis_show_ticks .~ True
      $ layout_plots .~ doPlot (zip ls defaultColours)
      $ def :: Layout Double Double 

    doPlot [] = []
    doPlot (((title,x),colour):xs) = (toPlot
                                      $ plot_lines_style .~ lineStyle colour 
                                      $ plot_lines_values .~ [x]  
                                      $ plot_lines_title .~ title
                                      $ def) : doPlot xs

plotB bar_titles bars title fn = renderableToFile def (barPlot title bar_titles bars) fn 
type Bars = [Double]
type BarClusters = ([String],[String],[Bars])

barPlot plot_title bar_titles bars =
  barClusterPlot plot_title (bar_titles,bar_titles, map (\x -> [x]) bars)

--barPlot :: String -> [BarCluster] -> Renderable something 
barClusterPlot plot_title bars = toRenderable layout
  where
    layout =
      layout_title .~ plot_title
      $ layout_title_style . font_size .~ 10
      $ layout_x_axis . laxis_generate .~ autoIndexAxis x_labels
      $ layout_y_axis . laxis_override .~ axisGridHide
      $ layout_left_axis_visibility . axis_show_ticks .~ False
      $ layout_plots .~ [plotBars (doPlot the_bars)]
      $ def :: Layout PlotIndex Double

    (x_labels,individuals,the_bars) = bars 
    
    doPlot bars =
      plot_bars_titles .~ individuals 
      $ plot_bars_values .~ addIndexes the_bars
      $ plot_bars_style .~ BarsClustered
      $ plot_bars_spacing .~ BarsFixGap 30 5
      $ plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq)
      $ def 
      
    mkstyle c = (solidFillStyle c, Nothing) -- bstyle)

-- plotExample = renderableToFile def (chart True) "example1.png"

-- chart borders = toRenderable layout
--  where
--   layout = 
--         layout_title .~ "Sample Bars" ++ btitle
--       $ layout_title_style . font_size .~ 10
--       $ layout_x_axis . laxis_generate .~ autoIndexAxis alabels
--       $ layout_y_axis . laxis_override .~ axisGridHide
--       $ layout_left_axis_visibility . axis_show_ticks .~ False
--       $ layout_plots .~ [ plotBars bars2 ]
--       $ def :: Layout PlotIndex Double

--   bars2 = plot_bars_titles .~ ["Cash","Equity"]
--       $ plot_bars_values .~ addIndexes [[20,45],[45,30],[30,20],[70,25]]
--       $ plot_bars_style .~ BarsClustered
--       $ plot_bars_spacing .~ BarsFixGap 30 5
--       $ plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq)
--       $ def

--   alabels = [ "Jun", "Jul", "Aug", "Sep", "Oct" ]

--   btitle = if borders then "" else " (no borders)"
--   bstyle = if borders then Just (solidLine 1.0 $ opaque black) else Nothing
--   mkstyle c = (solidFillStyle c, bstyle)






defaultColours = nub$ [ opaque red
                      , opaque green
                      , opaque blue
                      , opaque brown
                      , opaque magenta 
                      , opaque darkgreen
                      , opaque orange
                      ] ++ various 
  where
    various = [opaque $ sRGB r g b | r <- [0.1,0.2..1.0]
                                   , g <- [0.1,0.2..1.0]
                                   , b <- [0.1,0.2..1.0]]
                   
