module Lumi.Components.Examples.Highcharts where

import Prelude

import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import React.Basic (JSX, element)
import React.Basic.Highcharts (chartConfig, highcharts)

docs :: JSX
docs =
  column_
    [ example
        $ element highcharts
            { config: chartConfig
                { chart: { type: "bar" }
                , title: { text: "Fruit Consumption" }
                , xAxis: { categories: ["Apples", "Bananas", "Oranges"] }
                , yAxis: { title: { text: "Fruit eaten" } }
                , series:
                    [ { name: "Jane"
                      , data: [1, 0, 4]
                      }
                    , { name: "John"
                      , data: [5, 7, 3]
                      }
                    ]
                }
            }
    ]
