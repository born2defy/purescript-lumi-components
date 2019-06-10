module React.Basic.Highcharts where

import Prelude

import Effect (Effect)
import React.Basic.Hooks (ReactComponent, Ref)
import Type.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Chart :: Type

foreign import data ChartConfig :: Type

chartConfig :: forall a. Record a -> ChartConfig
chartConfig = unsafeCoerce

  -- { chart ::
  --     { type :: String
  --     }
  -- , title ::
  --     { text :: String
  --     }
  -- , xAxis ::
  --     { categories :: Array String
  --     }
  -- , yAxis ::
  --     { title ::
  --         { text :: String
  --         }
  --     }
  -- , series ::
  --     Array
  --       { name :: String
  --       , data :: Array Number
  --       }
  -- }


type HighchartsProps =
  ( config :: ChartConfig
  , isPureConfig :: Boolean
  , ref :: Ref Chart
  , callback :: Chart -> Effect Unit
  )

foreign import highcharts
  :: forall props props_
   . Union props props_ HighchartsProps
  => ReactComponent { | props }
