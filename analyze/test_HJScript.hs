{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import Data.Text.Lazy                (Text,pack)
import HJScript -- (HJScript,Exp,function,varWith,int,evalHJScript,functionDecl,call)
-- import HJScript.Objects.JQuery       hiding (prepend,append)
-- import HJScript.Objects.JQuery.Extra
import Prelude                       hiding ((++),max)

hdr :: String
hdr = "<html> <head> <script type=\"text/javascript\" src=\"https://www.google.com/jsapi\"></script> <script type=\"text/javascript\">"

ftr :: String
ftr = "    </script>  </head>  <body>  <div id=\"chart_div\" style=\"width: 900px; height: 500px;\"></div>  </body> </html>"

lns :: [String]
lns = [
  "      google.load(\"visualization\", \"1\", {packages:[\"corechart\"]});"
  ,"      google.setOnLoadCallback(drawChart);"
  ,"      function drawChart() {"
  ,"        var data = new google.visualization.DataTable();"
  ,"        data.addColumn('string', 'Year');"
  ,"        data.addColumn('number', 'Sales');"
  ,"        data.addColumn('number', 'Expenses');"
  ,"        data.addRows(["
  ,"          ['2004', 1000, 400],"
  ,"          ['2005', 1170, 460],"
  ,"          ['2006',  860, 580],"
  ,"          ['2007', 1030, 540]"
  ,"        ]);"
  ,"        var options = {"
  ,"          title: 'Company Performance'"
  ,"        };"
  ,"        var chart = new google.visualization.LineChart(document.getElementById('chart_div'));"
  ,"        chart.draw(data, options);"
  ,"      }"
  ]

testit :: HJScript ()
testit = do
  let googcall meth arg = callVoidMethod meth arg (TopLvl "google")

  functionDecl "drawChart" $ \ ()  -> do 
     -- I don't at all see how to use "new" here:
     dat <- varWith (JConst "new google.visualization.DataTable()")

     -- And why on earth is Var not valid for calling methods?
--     let datcall meth arg = callVoidMethod meth arg dat 
--     datcall "addColumn" ()

--         deref dat (JConst "blah") .=. string "hi"

--     return (x+1)
--     return (x)
     return (int 3)

  googcall "load" (string "visualization", string "1", JConst "{packages:[\"corechart\"]}")
  googcall "setOnLoadCallback" (JConst "drawChart")

  return ()


-- An unfortunate hack to satisfy confusing HJScript classes:
data TopLvl = TopLvl String
instance Show TopLvl where
  show (TopLvl s) = s
instance IsClass TopLvl


main = do
  putStrLn hdr
  putStrLn$ renderBlock $ snd $ evalHJScript $ testit 
  putStrLn ftr


{-

<html>
  <head>
    <script type="text/javascript" src="https://www.google.com/jsapi"></script>
    <script type="text/javascript">
      google.load("visualization", "1", {packages:["corechart"]});
      google.setOnLoadCallback(drawChart);
      function drawChart() {
        var data = new google.visualization.DataTable();
        data.addColumn('string', 'Year');
        data.addColumn('number', 'Sales');
        data.addColumn('number', 'Expenses');
        data.addRows([
          ['2004', 1000, 400],
          ['2005', 1170, 460],
          ['2006',  860, 580],
          ['2007', 1030, 540]
        ]);

        var options = {
          title: 'Company Performance'
        };

        var chart = new google.visualization.LineChart(document.getElementById('chart_div'));
        chart.draw(data, options);
      }
    </script>
  </head>
  <body>
    <div id="chart_div" style="width: 900px; height: 500px;"></div>
  </body>
</html>

-}





{-
<html>
  <head>
    <!--Load the AJAX API-->
    <script type="text/javascript" src="https://www.google.com/jsapi"></script>
    <script type="text/javascript">

      // Load the Visualization API and the piechart package.
      google.load('visualization', '1.0', {'packages':['corechart']});

      // Set a callback to run when the Google Visualization API is loaded.
      google.setOnLoadCallback(drawChart);

      // Callback that creates and populates a data table,
      // instantiates the pie chart, passes in the data and
      // draws it.
      function drawChart() {

        // Create the data table.
        var data = new google.visualization.DataTable();
        data.addColumn('string', 'Topping');
        data.addColumn('number', 'Slices');
        data.addRows([
          ['Mushrooms', 3],
          ['Onions', 1],
          ['Olives', 1],
          ['Zucchini', 1],
          ['Pepperoni', 2]
        ]);

        // Set chart options
        var options = {'title':'How Much Pizza I Ate Last Night',
                       'width':400,
                       'height':300};

        // Instantiate and draw our chart, passing in some options.
        var chart = new google.visualization.PieChart(document.getElementById('chart_div'));
        chart.draw(data, options);
      }
    </script>
  </head>

  <body>
    <!--Div that will hold the pie chart-->
    <div id="chart_div"></div>
  </body>
</html>
-}