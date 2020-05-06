module Lumi.Styles.Theme where

import Prelude

import Data.Int as Int
import Data.Newtype (class Newtype)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components.Color (Color, ColorMap, ColorName, colorNames, colors)
import React.Basic (ReactContext, createContext)
import React.Basic.Hooks (Hook, UseContext, useContext)

newtype LumiTheme
  = LumiTheme
  { colors :: ColorMap Color
  , colorNames :: ColorMap ColorName
  , fontSizes :: TextMap Int
  , lineHeightFactor :: Number
  , textMarginFactor :: Number
  }

type TextMap a =
  { body :: a
  , subtext :: a
  , subsectionHeader :: a
  , sectionHeader :: a
  , title :: a
  , mainHeader :: a
  }

derive instance newtypeLumiTheme :: Newtype LumiTheme _

defaultTheme :: LumiTheme
defaultTheme = LumiTheme
  { colors
  , colorNames
  , fontSizes:
      { body: 14
      , subtext: 12
      , subsectionHeader: 17
      , sectionHeader: 20
      , title: 24
      , mainHeader: 30
      }
  , lineHeightFactor: 17.0 / 14.0
  , textMarginFactor: 9.0 / 17.0
  }

lumiThemeContext :: ReactContext LumiTheme
lumiThemeContext =
  unsafePerformEffect do
    createContext defaultTheme

useTheme :: Hook (UseContext LumiTheme) LumiTheme
useTheme = useContext lumiThemeContext

textFontSize :: LumiTheme -> (forall a. TextMap a -> a) -> Int
textFontSize (LumiTheme { fontSizes }) selector = selector fontSizes

textLineHeight :: LumiTheme -> (forall a. TextMap a -> a) -> Int
textLineHeight (LumiTheme { fontSizes, lineHeightFactor }) selector =
  Int.floor $ Int.toNumber (selector fontSizes) * lineHeightFactor

textMargin :: LumiTheme -> (forall a. TextMap a -> a) -> Int
textMargin (LumiTheme { fontSizes, textMarginFactor }) selector =
  Int.floor $ Int.toNumber (selector fontSizes) * textMarginFactor
