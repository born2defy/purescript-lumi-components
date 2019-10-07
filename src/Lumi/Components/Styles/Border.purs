module Lumi.Components.Styles.Border where

import Prelude

import Color (cssStringHSLA)
import Lumi.Components.Color (Color, colors)
import React.Basic.DOM (CSS, css)

border :: Color -> CSS
border color =
  css
    { border: "1px solid " <> cssStringHSLA color
    }

border_ :: CSS
border_ = border colors.black4

borderRound :: CSS
borderRound = css { borderRadius: "3px" }

round :: CSS
round = css { borderRadius: "50%" }
