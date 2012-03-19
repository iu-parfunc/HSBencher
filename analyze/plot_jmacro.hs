{- |
Module      :  Language.Javascript.JMacro.Prelude
Copyright   :  (c) Gershom Bazerman, Jeff Polakow 2010
License     :  BSD 3 Clause
Maintainer  :  gershomb@gmail.com
Stability   :  experimental
-}

{-# LANGUAGE QuasiQuotes #-}

import Language.Javascript.JMacro

hdr :: String
hdr = "<html> <head> <script type=\"text/javascript\" src=\"https://www.google.com/jsapi\"></script> <script type=\"text/javascript\">"

ftr :: String
ftr = "    </script>  </head>  <body>  <div id=\"chart_div\" style=\"width: 900px; height: 500px;\"></div>  </body> </html>"


testdata :: [(String, Int, Int)]
testdata = [
      ("2004", 100, 400),
      ("2005", 1170, 460),
      ("2006",  860, 580),
      ("2007", 1030, 540)
    ]

-- | This provides a set of basic functional programming primitives, a few utility functions
-- and, more importantly, a decent sample of idiomatic jmacro code. View the source for details.
-- body :: JStat
body :: (ToJExpr a3, ToJExpr a2, ToJExpr a1, ToJExpr a) => (a, a1, a2) -> a3 -> JStat
body (title,line1,line2) testdata = [$jmacro|

  google.load("visualization", "1", {packages:["corechart"]});

  fun drawChart {
    var dat = new google.visualization.DataTable();
    dat.addColumn('string', `(title)` );
    dat.addColumn('number', `(line1)` );
    dat.addColumn('number', `(line2)` );

    // -- Here's our data... this can get BIG:
    dat.addRows( `(testdata)` );

    var options = {  title: `(title)` };
    var chart = new google.visualization.LineChart(document.getElementById('chart_div'));
    chart.draw(dat, options);
  }

  google.setOnLoadCallback(drawChart);
|]

main = do
  putStrLn hdr
  print$ renderJs$ body ("blah","line1","line2") testdata
  putStrLn ftr
