{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-} 

module HSBencher.Analytics where

import Numeric (showHex) 

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


-- A mixed plot 
data Plot x y =
  Plot { pLines  :: [LineGraph x y],
         pPoints :: [PointGraph x y], 
         pBars   :: [BarGraph x y],
         pLegend :: Bool,
         pDimensions :: (Int,Int),
         pXLabel :: String,
         pYLabel :: String 
       }
  deriving (Eq,Show, Read, Ord)

-- a plot supporting only stacked bargraphs
data BarStackPlot x y =
  BarStackPlot { bsStacks :: [[BarGraph x y]],
                 bsLegend :: Bool,
                 bsDimensions :: (Int,Int),
                 bsXLabel :: String,
                 bsYLabel :: String
               }
  deriving (Eq,Show, Read, Ord)

---------------------------------------------------------------------------
-- Exampleplots 
exampleLG :: Plot Double Double
exampleLG = Plot {pLines = [LineGraph "#F00"
                                      "Line1"
                                      Nothing
                                      [(x,x)|x <- [0..7]]],
                  pPoints = [],
                  pBars   = [],
                  pLegend = True,
                  pDimensions = (800,400),
                  pXLabel = "Threads",
                  pYLabel = "ms" }
examplePG :: Plot Double Double
examplePG = Plot {pPoints = [PointGraph "#F00"
                                        "Points1"
                                        "circle" 
                                        [(x,7-x)| x <- [0..7]]],
                  pLines = [],
                  pBars  = [],
                  pLegend = True,
                  pDimensions = (800,400),
                  pXLabel = "Threads",
                  pYLabel = "ms" }
            

exampleBG :: Plot Double Double
exampleBG = Plot {pBars = [BarGraph "#F00"
                                    "Bars1"
                                    [(x,x)| x <- [0..7]],
                           BarGraph "#0F0"
                                    "Bars2"
                                    [(x,(7-x))| x <- [0..7]],
                           BarGraph "#00F"
                                    "Bars3"
                                    [(x,5.5)| x <- [0..7]]],
                  pLines = [],
                  pPoints = [],
                  pLegend = True,
                  pDimensions = (800,400),
                  pXLabel = "Threads",
                  pYLabel = "ms"
                 }


exampleMG :: Plot Int Double
exampleMG = Plot {pBars = [BarGraph "#F00"
                                    "Bars1"
                                    [(x,fromIntegral x)| x <- [0..7]],
                           BarGraph "#0F0"
                                    "Bars2"
                                    [(x,fromIntegral (7-x))| x <- [0..7]],
                           BarGraph "#00F"
                                    "Bars3"
                                    [(x,5)| x <- [0..7]]],
                  pLines = [LineGraph "#000"
                                      "Line1"
                                      Nothing
                                      [(x,4+sin (fromIntegral x))|x <- [0..7]]],

                  pPoints = [],
                  pLegend = True,
                  pDimensions = (800,400),
                  pXLabel = "Threads",
                  pYLabel = "ms"
                 }

exampleStack :: BarStackPlot String Double
exampleStack =
  BarStackPlot { bsStacks = [[BarGraph "#F00"
                                       "Dynaprof"
                                       [("gcc",4.5),("tar",3.3)],
                              BarGraph "#F07"
                                       "Dynaprof startup"
                                       [("gcc",1.2),("tar",1.2)]],
                             [BarGraph "#00F"
                                       "Leading contender"
                                       [("gcc",5.5),("tar",4.3)],
                              BarGraph "#07F"
                                       "Leading contender startup"
                                       [("gcc",0.7),("tar",0.7)]]],
                 bsLegend = True,
                 bsDimensions = (800,400),
                 bsXLabel = "Program",
                 bsYLabel = "Time"
               } 
                             

---------------------------------------------------------------------------
-- Utilities 

hexcolors = ["#"++h r++h g++h b | r <- [0..15] , g <- [0..15], b <- [0..15]]
  where
    h x = showHex x ""


---------------------------------------------------------------------------
--

-- TODO: Look for a library that can generate JS code for me.
-- And HTML (in a quasiquotation kind of way).
    
mySupply :: Supply Int 
mySupply = unsafePerformIO $ newEnumSupply 

class Plotable a where
  toPlot :: a -> String
  plotKind :: a -> String

instance Plotable String where
  toPlot str = show str -- want the extra " "
  plotKind str = "mode: \"categories\""  

instance Plotable Double where
  toPlot d = show d
  plotKind d = ""

instance Plotable Int where
  toPlot i = show i
  plotKind i = "tickDecimals: 0" 
  
---------------------------------------------------------------------------
{-
   Plotting stacked bars is a bit tricky.
   Mixing plotting of stacked bars and nonstacked seems impossible.
   May be possible to treat nonstacked bars as a special case of stacked ones. 

-} 
renderPlot :: forall x y . (Plotable x, Plotable y)
              => Supply Int -> Plot x y -> String
renderPlot s pl =
  "$(function () { \n" ++
   plotOptions ++ "\n" ++ 
   body ++ plot charts ++ 
  "});"
  
  where
    Plot lines
         points
         bars
         legend
         (width,height)
         xlabel
         ylabel = pl
     
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

    order = [1..] :: [Int]
    nBarGraphs = length bars
    barWidth = 1 / (fromIntegral (nBarGraphs + 1)) :: Double -- +1 for space

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
    chartBars bw b  [] = error "chartBars: not matching!"
    chartBars bw [] b  = error "chartBars: not matching!"
    chartBars bw (v:vs) ((o,b):bs) = 
         ("{\n data: " ++ v ++ ",\n" ++
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
        
    pngButton
      = "document.getElementById(\"toPNGButton\").onclick = function (somePlot) {\n" ++
        "var canvas = someplot.getCanvas();\n" ++
        "var ctx = canvas.getContext(\"2d\");\n" ++
        "window.open(canvas.toDataURL('png'), \"\");\n" ++
        "}"
        
--------------------------------------------------------------------------- 
-- Obtain variables and code from a dataset.
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
-- Render a bargraph with stacked bars.. 
renderBSPlot :: forall x y . (Plotable x, Plotable y)
              => Supply Int -> BarStackPlot x y -> String
renderBSPlot s pl =
  "$(function () { \n" ++
   plotOptions ++ "\n" ++ 
   body ++ plot chart ++ 
  "});"
  
  where
    BarStackPlot bars
         legend
         (width,height)
         xlabel
         ylabel = pl
     
    -- (s1:s2:_) = split s 
    (v,code) = barStackDataSet s bars --- (map bgData bars)
    
    plot c = "var someplot = $.plot(\"#placeholder\", [" ++ 
             c ++
             "], options); \n" ++
             pngButton
             
    body = code

    chart' = map (concat .intersperse ", ") $ chartBars v (zip order bars)
    chart =  (concat (intersperse "} , {" chart')) 
    
    order = [1..] :: [Int]
    nBarGraphs = length bars
    barWidth = 1 / (fromIntegral (nBarGraphs + 1)) :: Double -- +1 for space

    chartBars :: [[String]] -> [(Int,[BarGraph x y])] -> [[String]] 
    chartBars [] [] = []
    chartBars b  [] = error "chartBars: not matching!"
    chartBars [] b  = error "chartBars: not matching!"
    chartBars (v:vs) ((o,b):bs) =
      generateStack v b : chartBars vs bs
       where
         generateStack [] [] = []
         generateStack (v:vs) (b:bs) = 
      
           ("{\n data: " ++ v ++ ",\n" ++
            "bars: {show: true," ++
            "order: " ++ show o ++ ",\n" ++
            "barWidth: " ++ show barWidth ++ "},\n" ++
            "label: " ++ show (bgLabel b) ++ ",\n" ++
            "color: " ++ show (bgColor b) ++ "\n" ++
            "}")  : generateStack vs bs
    
    plotOptions
      = "var options = {canvas: true," ++ 
                       "legend: {position: \"nw\", type: \"canvas\" }," ++
                       "axisLabels: {show: true}," ++
                       "xaxis: {axisLabel: " ++ show xlabel ++ ", axisLabelUseCanvas: true, " ++ plotKind (undefined :: x) ++ " }," ++
                       "yaxis: {axisLabel: " ++ show ylabel ++ ", axisLabelUseCanvas: true, " ++ plotKind (undefined :: y) ++ " }};" 
        
    pngButton
      = "document.getElementById(\"toPNGButton\").onclick = function (somePlot) {\n" ++
        "var canvas = someplot.getCanvas();\n" ++
        "var ctx = canvas.getContext(\"2d\");\n" ++
        "window.open(canvas.toDataURL('png'), \"\");\n" ++
        "}" 

barStackDataSet :: (Plotable x, Plotable y)
                   => Supply Int -> [[BarGraph x y]] -> ([[String]],String)
barStackDataSet s [] = ([],[])
barStackDataSet s (x:xs) = (v:vs, code ++ c)
  where
    (s1,s2) = split2 s 
    (v,code) = dataSet s1 (map bgData x)
    (vs,c)   = barStackDataSet s2 xs 

-- dataSet :: (Plotable x, Plotable y)
--            => Supply Int -> [[(x,y)]] -> ([String], String)
-- dataSet _ [] = ([],[])
-- dataSet s (x:xs) = (vn:vars, def ++ code) 
--   where
--     vn = "v" ++ show (supplyValue s )
--     (s1,s2) = split2 s 
--     def = "var " ++ vn ++ " = [" ++ dataValues x ++ "];\n"
--     (vars,code) = dataSet s2 xs
--     dataValues x = concat $ intersperse "," $ map tupToArr x
--     tupToArr (x,y) = "[ " ++ toPlot x ++ ", " ++ toPlot y ++ "]" 



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
  "<div id=\"header\"> <h2>Generated Plot!</h2> </div>\n" ++
  "<div id=\"content\">\n" ++ 
  "<div id=\"placeholder\" style=\"width:800px;height:400px\"></div>\n" ++ 
  "<div id=\"button\"> <input id=\"toPNGButton\" type=\"button\" value=\"getPNG\" /> </div> \n" ++
  "</div>\n" ++
  "<div id=\"footer\"> HTML and Plot generated by HSBencher. </div>\n" ++
  "</body>\n" ++
  "</html>\n"
