module Lumi.Components2.Examples.QRCode where

import Prelude
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (lumiElement)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Input as Input
import Lumi.Components.Row (row_)
import Lumi.Components.Spacing (Space(..), hspace, vspace)
import Lumi.Components.Text (body_, subsectionHeader_)
import Lumi.Components2.Box (box)
import Lumi.Components2.Button (button, _secondary)
import Lumi.Components2.Link (link)
import Lumi.Components2.QRCode (ErrorCorrectLevel(..), useQRCode)
import Lumi.Styles as S
import Lumi.Styles.Border as Border
import Lumi.Styles.Box (FlexAlign(..))
import Lumi.Styles.Box as Box
import Lumi.Styles.Button (ButtonState(..))
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, targetValue)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\))
import React.Basic.Hooks as React
import Web.HTML.History (URL(..))

docs :: JSX
docs =
  flip element {}
    $ unsafePerformEffect do
        component "QRCodeExample" \_ -> React.do
          value /\ setValue <- useState "https://www.lumi.com"
          pure
            $ lumiElement box
                _
                  { content =
                    [ Input.input
                        Input.text_
                          { value = value
                          , onChange = capture targetValue $ traverse_ (const >>> setValue)
                          }
                    , vspace S24
                    , example
                        $ element qrcodeExample { value }
                    , example
                        $ element qrcodeExample_ { value }
                    ]
                  }

qrcodeExample :: ReactComponent { value :: String }
qrcodeExample =
  unsafePerformEffect do
    component "QRCode" \props -> React.do
      { qrcode, url } <- useQRCode ECLLow props.value
      pure
        $ lumiElement box
        <<< Box._align Center
        $ _
            { content =
              [ lumiElement qrcode
                  <<< Border.border
                  >>> Border._round
                  >>> S.styleModifier_
                      ( S.css
                          { padding: S.int 16
                          , width: S.int 140
                          }
                      )
                  $ identity
              , vspace S8
              , lumiElement link
                  _
                    { href = fromMaybe (URL "") url
                    , download = Just "qrcode.svg"
                    , content =
                      [ subsectionHeader_ "Download SVG"
                      ]
                    }
              ]
            }

qrcodeExample_ :: ReactComponent { value :: String }
qrcodeExample_ =
  unsafePerformEffect do
    component "QRCode" \props -> React.do
      { qrcode, url } <- useQRCode ECLLow props.value
      pure
        $ lumiElement box
        <<< Box._align Center
        $ _
            { content =
              [ row_
                  [ lumiElement qrcode
                      $ Border.border
                      $ Border._round
                      $ S.styleModifier_
                          ( S.css
                              { padding: S.int 16
                              , width: S.int 140
                              }
                          )
                      $ identity
                  , hspace S24
                  , column_
                      [ body_ "Lorem ipsum"
                      , vspace S8
                      , lumiElement button
                          $ _secondary
                          $ _
                              { content = [ R.text "Download SVG" ]
                              , state = Enabled
                              }
                      ]
                  ]
              ]
            }
