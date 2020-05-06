module Lumi.Components2.Text
  ( Text
  , TextProps, TextType
  , text
  , TextModifier, body, strong, emphasized, subtext
  , muted, color

  , ParagraphProps
  , paragraph_, paragraph

  , Header
  , HeaderProps
  , subsectionHeader
  , sectionHeader
  , title
  , mainHeader
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, PropsModifier, PropsModifier', lumiComponent, propsModifier)
import Lumi.Components.Color (ColorMap)
import Lumi.Styles (Style, StyleModifier, StyleProperty)
import Lumi.Styles as S
import Lumi.Styles.Theme (LumiTheme(..), TextMap, textFontSize, textLineHeight, textMargin, useTheme)
import React.Basic (JSX, ReactComponent)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks as Hooks

-- Text

data Text = Text

data TextType
  = Body
  | Strong
  | Emphasis
  | Subtext

type TextProps =
  ( component :: Text
  , type :: Maybe TextType
  , content :: String
  )

type TextElement = ReactComponent { children :: Array JSX, className :: String }

-- | TODO: write documentation
text :: LumiComponent TextProps
text =
  unsafePerformEffect $ lumiComponent "Text" defaults \props -> Hooks.do
    theme <- useTheme
    pure $
      E.element (maybe R.span' textElement props.type)
        { children: [ R.text props.content ]
        , className: props.className
        , css: defaultTextStyle theme props.type <> props.css theme
        }
  where
    defaults :: Record TextProps
    defaults =
      { component: Text
      , type: Nothing
      , content: ""
      }

    defaultTextStyle :: LumiTheme -> Maybe TextType -> Style
    defaultTextStyle theme ty =
      S.css
        { fontSize: maybe S.inherit (px <<< textFontSize theme <<< textTheme) ty
        , lineHeight: maybe S.inherit (px <<< textLineHeight theme <<< textTheme) ty
        , whiteSpace: S.str "pre-wrap"
        , margin: S.str "0"
        , padding: S.str "0"
        }

    textElement :: TextType -> TextElement
    textElement Body = R.span'
    textElement Strong = R.strong'
    textElement Emphasis = R.em'
    textElement Subtext = R.small'

textTheme :: TextType -> (forall a. TextMap a -> a)
textTheme =
  case _ of
    Body -> _.body
    Strong -> _.body
    Emphasis -> _.body
    Subtext -> _.subtext

-- | The `c` type parameter lets us constrain the type of component to which
-- | a text modifier may be applied: `Text`, `Header` or any.
type TextModifier c = forall r. PropsModifier (component :: c, type :: Maybe TextType | r)

body :: TextModifier Text
body = propsModifier _{ type = Just Body }

strong :: forall c. TextModifier c
strong =
  propsModifier _{ type = Just Strong }
  <<< S.style_ (S.css { fontWeight: S.str "600" })

emphasized :: forall c. TextModifier c
emphasized =
  propsModifier _{ type = Just Emphasis }
  <<< S.style_ (S.css { fontStyle: S.str "italic" })

subtext :: TextModifier Text
subtext = propsModifier _{ type = Just Subtext }

muted :: forall c. TextModifier c
muted =
  S.style \(LumiTheme { colors }) ->
    S.css { color: S.color colors.black1 }

color :: forall c. (forall a. ColorMap a -> a) -> TextModifier c
color f =
  S.style \(LumiTheme { colors }) ->
    S.css { color: S.color (f colors) }

-- Paragraph

type ParagraphProps =
  ( component :: Text
  , type :: Maybe TextType
  , content :: Array JSX
  )

-- | TODO: write documentation
paragraph_ :: LumiComponent TextProps
paragraph_ = paragraph <<< renderInnerText
  where
    renderInnerText :: PropsModifier' TextProps ParagraphProps
    renderInnerText f props =
      let
        props' = f $ props { content = "" }
      in
        props'
          { content =
              [ text _
                  { component = props'.component
                  , type = props'.type
                  , content = props'.content
                  , css = \_ -> S.css { fontSize: S.inherit, lineHeight: S.inherit }
                  }
              ]
          }

-- | TODO: write documentation
paragraph :: LumiComponent ParagraphProps
paragraph =
  unsafePerformEffect $ lumiComponent "Paragraph" defaults \props -> Hooks.do
    theme <- useTheme
    pure $
      E.element R.p'
        { children: props.content
        , className: props.className
        , css: defaultParagraphStyle theme props.type <> props.css theme
        }
  where
    defaults :: Record ParagraphProps
    defaults =
      { component: Text
      , type: Nothing
      , content: []
      }

    defaultParagraphStyle :: LumiTheme -> Maybe TextType -> Style
    defaultParagraphStyle theme ty =
      S.merge
        [ S.css
            { whiteSpace: S.str "pre-wrap"
            , margin: S.str "0"
            , padding: S.str "0"
            }
        , S.toCSS (textStyle (textTheme (fromMaybe Body ty))) theme
        ]

-- Headers

data Header = Header

-- Even though the header components never end up using the `type` property, we
-- need it here so that text modifiers such as `strong` may also be applied to
-- headers.
type HeaderProps =
  ( component :: Header
  , type :: Maybe TextType
  , content :: String
  )

subsectionHeader :: LumiComponent HeaderProps
subsectionHeader = mkHeaderComponent R.h4' <<< textStyle _.subsectionHeader

sectionHeader :: LumiComponent HeaderProps
sectionHeader = mkHeaderComponent R.h3' <<< textStyle _.sectionHeader

title :: LumiComponent HeaderProps
title = mkHeaderComponent R.h2' <<< textStyle _.title

mainHeader :: LumiComponent HeaderProps
mainHeader = mkHeaderComponent R.h1' <<< textStyle _.mainHeader

-- | TODO: write documentation
mkHeaderComponent
  :: TextElement
  -> LumiComponent HeaderProps
mkHeaderComponent el =
  unsafePerformEffect $ lumiComponent "Header" defaults \props -> Hooks.do
    theme <- useTheme
    pure $
      E.element el
        { children: [ R.text props.content ]
        , className: props.className
        , css: defaultHeaderStyle theme <> props.css theme
        }
  where
    defaults :: Record HeaderProps
    defaults =
      { component: Header
      , type: Nothing
      , content: ""
      }

    defaultHeaderStyle :: LumiTheme -> Style
    defaultHeaderStyle theme =
      S.merge
        [ S.css
            { fontWeight: S.str "400"
            , padding: S.str "0"
            , margin: S.str "0"
            }
        , S.toCSS (textStyle _.subsectionHeader) theme
        ]

--

textStyle :: (forall a. TextMap a -> a) -> StyleModifier
textStyle selector =
  S.style \theme -> 
    S.css
      { fontSize: px $ textFontSize theme selector
      , lineHeight: px $ textLineHeight theme selector
      , marginBottom: px $ textMargin theme selector
      }

px :: Int -> StyleProperty
px i = S.str $ show i <> "px"