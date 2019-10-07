module Lumi.Components.Styles.Layout
  ( fill
  , center
  , column
  , row
  , margin
  , margin_
  , padding
  , padding_
  ) where

import Lumi.Components.Orientation (Orientation(..))
import Lumi.Components.Size (Size(..))
import React.Basic.DOM (CSS, css)

fill :: CSS
fill = css { flex: "1" }

center :: CSS
center =
  css
    { display: "flex"
    , alignItems: "center"
    , justifyContent: "center"
    }

column :: CSS
column = css { display: "flex", flexDirection: "column" }

row :: CSS
row = css { display: "flex", flexDirection: "row" }

margin :: Orientation -> Size -> CSS
margin o s =
  case o of
    Horizontal ->
      css { marginLeft: sizeFor s, marginRight: sizeFor s }
    Vertical ->
      css { marginTop: sizeFor s, marginBottom: sizeFor s }

margin_ :: Size -> CSS
margin_ s = css { margin: sizeFor s }

padding :: Orientation -> Size -> CSS
padding o s =
  case o of
    Horizontal ->
      css { paddingLeft: sizeFor s, paddingRight: sizeFor s }
    Vertical ->
      css { paddingTop: sizeFor s, paddingBottom: sizeFor s }

padding_ :: Size -> CSS
padding_ s = css { padding: sizeFor s }

--

sizeFor :: Size -> String
sizeFor Small = "4px"
sizeFor Medium = "8px"
sizeFor Large = "16px"
sizeFor ExtraLarge = "24px"
