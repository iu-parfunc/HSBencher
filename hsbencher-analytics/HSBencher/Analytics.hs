{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-} 

module HSBencher.Analytics where



--import Graphics.Rendering.Chart
--import Graphics.Rendering.Chart.Backend.Cairo

--import Data.Colour
--import Data.Colour.Names
--import Data.Colour.SRGB
--import Data.Default.Class
-- import Control.Lens
import Numeric 

import Data.Supply

import HSBencher.Internal.Fusion

import System.IO.Unsafe

import Data.List hiding (init, lines)
import Prelude hiding (init, lines) 

---------------------------------------------------------------------------
--

pullEntireTable cid sec table_name = do
  (table_id,auth) <- init cid sec table_name
  getSomething auth table_id "*"
  

---------------------------------------------------------------------------
--
{- 
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
                   


-} 

---------------------------------------------------------------------------
-- Simple ploting with Flot 

-- Kinds of graphs 
data LineGraph x y =
  LineGraph {lgColor :: String,   -- for now
             lgLabel :: String, -- What the legend should say.
             lgSymbol :: Maybe String,  
             lgData  :: [(x,y)]} -- data to plot as a line.
  deriving (Eq,Show, Read, Ord)

data PointGraph x y =
  PointGraph {pgColor :: String,
              pgLabel :: String,
              pgSymbol :: String, 
              pgData :: [(x,y)]}
  deriving (Eq,Show, Read, Ord)

data BarGraph x y =
  BarGraph {bgColor :: String,
            bgLabel :: String,
            bgData :: [(x,y)]}
  deriving (Eq,Show, Read, Ord)

           
data Plot x y =
  Plot { lines  :: [LineGraph x y],
         points :: [PointGraph x y], 
         bars   :: [BarGraph x y],
         legend :: Bool,

         dimensions :: (Int,Int),
         xLabel :: String,
         yLabel :: String 
       }
  deriving (Eq,Show, Read, Ord)


---------------------------------------------------------------------------
-- Exampleplots 
exampleLG :: Plot Double Double
exampleLG = Plot {lines = [LineGraph "#F00"
                                     "Line1"
                                     Nothing
                                     [(x,x)|x <- [0..7]]],
                  points = [],
                  bars   = [],
                  legend = True,
                  dimensions = (800,400),
                  xLabel = "Threads",
                  yLabel = "ms" }
examplePG :: Plot Double Double
examplePG = Plot {points = [PointGraph "#F00"
                                       "Points1"
                                       "circle" 
                                       [(x,7-x)| x <- [0..7]]],
                  lines = [],
                  bars  = [],
                  legend = True,
                  dimensions = (800,400),
                  xLabel = "Threads",
                  yLabel = "ms" }
            

exampleBG :: Plot Double Double
exampleBG = Plot {bars = [BarGraph "#F00"
                                   "Bars1"
                                   [(x,x)| x <- [0..7]],
                          BarGraph "#0F0"
                                   "Bars2"
                                   [(x,(7-x))| x <- [0..7]],
                          BarGraph "#00F"
                                   "Bars3"
                                   [(x,5.5)| x <- [0..7]]],
                  lines = [],
                  points = [],
                  legend = True,
                  dimensions = (800,400),
                  xLabel = "Threads",
                  yLabel = "ms"
                 }


exampleMG :: Plot Int Double
exampleMG = Plot {bars = [BarGraph "#F00"
                                   "Bars1"
                                   [(x,fromIntegral x)| x <- [0..7]],
                          BarGraph "#0F0"
                                   "Bars2"
                                   [(x,fromIntegral (7-x))| x <- [0..7]],
                          BarGraph "#00F"
                                   "Bars3"
                                   [(x,5)| x <- [0..7]]],
                  lines = [LineGraph "#000"
                                     "Line1"
                                     Nothing
                                     [(x,4+sin (fromIntegral x))|x <- [0..7]]],

                  points = [],
                  legend = True,
                  dimensions = (800,400),
                  xLabel = "Threads",
                  yLabel = "ms"
                 }



             

---------------------------------------------------------------------------
-- Utilities 

hexcolors = ["#"++h r++h g++h b | r <- [0..15] , g <- [0..15], b <- [0..15]]
  where
    h x = showHex x ""


---------------------------------------------------------------------------
-- RenderPlot

-- TODO: Look for a library that can generate JS code for me.
-- And HTML (in a quasiquotation kind of way).
    
mySupply :: Supply Int 
mySupply = unsafePerformIO $ newEnumSupply 

class Plotable a where
  toPlot :: a -> String
  plotKind :: a -> String

instance Plotable String where
  toPlot str = show str -- want the extra " "
  plotKind str = ""  

instance Plotable Double where
  toPlot d = show d
  plotKind d = ""

instance Plotable Int where
  toPlot i = show i
  plotKind i = "tickDecimals: 0" 
  

renderPlot :: forall x y . (Plotable x, Plotable y) => Supply Int -> Plot x y -> String
renderPlot s pl =
  "$(function () { \n" ++
   plotOptions ++ "\n" ++ 
   body ++ plot charts ++ 
  "});"
  
  where
    Plot lines points bars legend (width,height) xlabel ylabel = pl
     
    (s1:s2:s3:_) = split s 
    (v1,code1) = dataSet s1 (map lgData lines) 
    (v2,code2) = dataSet s2 (map pgData points)
    (v3,code3) = dataSet s3 (map bgData bars) 
    plot c = "var someplot = $.plot(\"#placeholder\", [" ++ 
             c ++
             "], options); \n" ++
             pngButton
             
             
    body = code1 ++ code2 ++ code3 

    charts = (concat $ intersperse ", " $ chartLines v1 lines) ++
             (concat $ intersperse ", " $ chartBars barWidth v3 (zip order bars))

    order = [1..]
    nBarGraphs = length bars
    barWidth = 1 / (fromIntegral (nBarGraphs + 1)) -- +1 for space

    chartLines [] [] = [""]
    chartLines c [] = error $ "chartLines: not matching!"
    chartLines [] c = error $ "chartLines: not matching!"
    chartLines (v:vs) (l:ls)
      = ("{\n data: "++ v ++ ",\n" ++
         "lines: { show: true, fill: false },\n" ++
         "label: " ++ show (lgLabel l) ++ ",\n" ++
         "color: " ++ show (lgColor l) ++ "\n" ++
         "}") :  chartLines vs ls

    chartBars bw [] [] = [""]
    chartBars bw b  [] = error "chartBoxes: not matching!"
    chartBars bw [] b  = error "chartBoxes: not matching!"
    chartBars bw (v:vs) ((o,b):bs)
      = ("{\n data: " ++ v ++ ",\n" ++
         "bars: {show: true," ++
                "order: " ++ show o ++ ",\n" ++
                "barWidth: " ++ show bw ++ "},\n" ++
         "label: " ++ show (bgLabel b) ++ ",\n" ++
         "color: " ++ show (bgColor b) ++ "\n" ++
         "}") : chartBars bw vs bs 
    
    plotOptions
      = "var options = {canvas: true," ++ 
                       "legend: {position: \"nw\", type: \"canvas\" }," ++
                       "axisLabels: {show: true}," ++
                       "xaxis: {axisLabel: " ++ show xlabel ++ ", axisLabelUseCanvas: true, " ++ plotKind (undefined :: x) ++ " }," ++
                       "yaxis: {axisLabel: " ++ show ylabel ++ ", axisLabelUseCanvas: true, " ++ plotKind (undefined :: y) ++ " }};" 
    -- xaxis_kind =
    --   case plotKind (undefined :: x) of
    --     PlotCategories -> ""
    --     PlotDouble     -> ""
    --     PlotInt        -> "tickDecimals: 0"

    -- yaxis_kind =
    --   case plotKind (undefined :: x) of
    --     PlotCategories -> ""
    --     PlotDouble     -> ""
    --     PlotInt        -> "tickDecimals: 0"
    
                       
    pngButton
      = "document.getElementById(\"toPNGButton\").onclick = function (somePlot) {\n" ++
        "var canvas = someplot.getCanvas();\n" ++
        "var ctx = canvas.getContext(\"2d\");\n" ++
        "window.open(canvas.toDataURL('png'), \"\");\n" ++
        "}" 
    
dataSet :: (Plotable x, Plotable y)
           => Supply Int -> [[(x,y)]] -> ([String], String)
dataSet _ [] = ([],[])
dataSet s (x:xs) = (vn:vars, def ++ code) 
  where
    vn = "v" ++ show (supplyValue s )
    (s1,s2) = split2 s 
    def = "var " ++ vn ++ " = [" ++ dataValues x ++ "];\n"
    (vars,code) = dataSet s2 xs
    dataValues x = concat $ intersperse "," $ map tupToArr x
    tupToArr (x,y) = "[ " ++ toPlot x ++ ", " ++ toPlot y ++ "]" 




---------------------------------------------------------------------------
-- HTML   
html js =
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n" ++
  "<html>\n" ++
  "<head>\n" ++
  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">\n" ++
  "<title>Generated Plot</title>\n" ++
  "<link href=\"plot.css\" rel=\"stylesheet\" type=\"text/css\">\n" ++
  "<script language=\"javascript\" type=\"text/javascript\" src=\"./flot/jquery.js\"></script>\n" ++ 
  "<script language=\"javascript\" type=\"text/javascript\" src=\"./flot/jquery.flot.js\"></script>\n" ++ 
  "<script language=\"javascript\" type=\"text/javascript\" src=\"./flot/jquery.flot.canvas.js\"></script>\n" ++
  "<script language=\"javascript\" type=\"text/javascript\" src=\"./flot/jquery.flot.categories.js\"></script>\n" ++
  "<script language=\"javascript\" type=\"text/javascript\" src=\"./flot/jquery.flot.symbol.js\"></script>\n" ++
  "<script language=\"javascript\" type=\"text/javascript\" src=\"./flot/jquery.flot.legendoncanvas.js\"></script>\n" ++
  "<script language=\"javascript\" type=\"text/javascript\" src=\"./flot/jquery.flot.orderBars.js\"></script>\n" ++
  "<script language=\"javascript\" type=\"text/javascript\" src=\"./flot/jquery.flot.axislabels.js\"></script>\n" ++ 
  "<script type=\"text/javascript\">\n" ++ 
  js ++
  "</script>\n" ++
  "</head>\n" ++
  "<body>\n" ++
  "<div id=\"header\"> <h2>TESTING!</h2> </div>\n" ++
  "<div id=\"content\">\n" ++ 
  "<div id=\"placeholder\" style=\"width:800px;height:400px\"></div>\n" ++ 
  "<div id=\"button\"> <input id=\"toPNGButton\" type=\"button\" value=\"getPNG\" /> </div> \n" ++
  "</div>\n" ++
  "<div id=\"footer\"> HTML and Plot generated by HSBencher. </div>\n" ++
  "</body>\n" ++
  "</html>\n"
