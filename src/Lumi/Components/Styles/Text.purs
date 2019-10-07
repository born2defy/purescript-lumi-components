module Lumi.Components.Styles.Text where

import Color (cssStringHSLA)
import Lumi.Components.Color (colors)
import React.Basic.DOM (CSS, css)

ellipsis :: CSS
ellipsis =
  css
    { whiteSpace: "nowrap"
    , overflow: "hidden"
    , textOverflow: "ellipsis"
    }

muted :: CSS
muted =
  css
    { color: cssStringHSLA colors.black1
    }
