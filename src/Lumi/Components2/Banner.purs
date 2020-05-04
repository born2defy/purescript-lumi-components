module Lumi.Components2.Banner
  ( Banner
  , BannerProps
  , banner
  , actionBanner
  , primary
  , active
  , warning
  , error
  , module Styles.Banner
  ) where

import Prelude

import Color (lighten)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid as Monoid
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, PropsModifier, lumiComponent)
import Lumi.Components.Icon as Icon
import Lumi.Components.Spacing (Space(..))
import Lumi.Components2.Box (box)
import Lumi.Styles (color, css, int, nested, prop, str, style, style_, toCSS)
import Lumi.Styles.Banner (banner) as S
import Lumi.Styles.Border (_listSpaced, _listCompact) as Styles.Banner
import Lumi.Styles.Box (FlexAlign(..), _align, _alignSelf, _column, _interactive, _justify, _row) as S
import Lumi.Styles.Responsive (desktopQuery)
import Lumi.Styles.Responsive (onDesktop) as S
import Lumi.Styles.Theme (LumiTheme(..), useTheme)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (useState)
import React.Basic.Hooks as React

data Banner = Banner

type BannerProps =
  ( component :: Banner
  , dismissable :: Boolean
  , icon :: Maybe JSX
  , title :: Maybe JSX
  , content :: Array JSX
  )

banner :: LumiComponent BannerProps
banner =
  unsafePerformEffect do
    lumiComponent "Banner" defaults \props -> React.do
      theme <- useTheme
      visible /\ setVisible <- useState true
      pure
        $ Monoid.guard visible
        $ box
        $ style props.css
        $ S.banner
        $ _ { className = props.className
            , content =
                [ box
                  $ S._column
                  $ S._alignSelf S.Center
                  $ style_
                      ( css
                        { flex: str "1", width: str "100%" }
                      )
                  $ _ { content =
                          [ case props.title of
                              Just title ->
                                box
                                $ S._alignSelf S.Start
                                $ style_
                                    ( css
                                      { marginBottom: int 8
                                      , maxWidth: str "calc(100% - 24px)"
                                      }
                                    )
                                $ _ { content = [ title ]
                                    }
                              _ -> mempty
                          , box
                            $ S._align S.Center
                            $ S._row
                            $ style_
                                ( css
                                  { flex: str "1" }
                                )
                            $ _ { content =
                                    [ case props.icon of
                                        Just icon ->
                                          box
                                          $ S._alignSelf S.Start
                                          $ S.onDesktop (S._alignSelf S.Center)
                                          $ style_
                                              ( fold
                                                  [ css
                                                      { marginRight: int 16
                                                      }
                                                  , desktopQuery $ css
                                                      { alignSelf: str "center"
                                                      }
                                                  ]
                                              )
                                          $ _ { content = [ icon ]
                                              }
                                        _ -> mempty
                                    ]
                                    <> props.content
                                }
                          ]
                      }
                ,  Monoid.guard props.dismissable
                    $ E.element R.div'
                      { children:
                          [ Icon.icon
                              { style: R.css { fontSize: "12px" }
                              , type_: Icon.Remove
                              }
                          ]
                      , className: ""
                      , css: theme # toCSS dismissButtonStyle
                      , onClick: capture_ $ setVisible \_ -> false
                      }
                ]
            }
  where
    defaults :: Record BannerProps
    defaults =
      { component: Banner
      , title: Nothing
      , content: []
      , dismissable: false
      , icon: Nothing
      }

    dismissButtonStyle :: forall props. PropsModifier props
    dismissButtonStyle =
      S._interactive
      <<< style \(LumiTheme { colors }) ->
            css
              { position: str "absolute"
              , top: int 16
              , right: int 24
              , "&:hover": nested $ css
                  { color: color colors.black1
                  }
              }

actionBanner :: Array JSX -> PropsModifier BannerProps
actionBanner actions f =
  f >>> \props -> props
    { content =
        [ box
          $ S._column
          $ style_
              ( css
                { flex: str "1" }
              )
          $ S.onDesktop (S._row)
          $ style_
              ( fold
                  [ css
                      { alignItems: str "flex-end"
                      }
                  , desktopQuery $ css
                      -- @NOTE positioning of the action button changes dependent on if there is a banner title or not
                      { alignItems: if (isJust props.title)
                          then str "flex-end"
                          else str "center"
                      }
                  ]
              )
          $ _ { content =
                  [ box
                    $ style_
                        ( css
                          { flex: str "1" }
                        )
                    $ _ { content = props.content }
                  , box
                    $ S._row
                    $ S._align S.Center
                    $ S._justify S.End
                    $ style_
                        ( fold
                            [ css
                                { margin: str "16px 0 0"
                                , "& :not(:first-child)": nested $ css
                                    { marginLeft: prop S8
                                    }
                                }
                            , desktopQuery $ css
                                { margin: str "0 0 0 40px"
                                }
                            ]
                        )
                    $ _ { content = actions }
                  ]
              }
        ]
    }

primary :: forall props. PropsModifier (component :: Banner | props)
primary =
  style \(LumiTheme { colors }) ->
    css
      { backgroundColor: color colors.primary3
      }

active :: forall props. PropsModifier (component :: Banner | props)
active =
  style \(LumiTheme { colors }) ->
    css
      { backgroundColor: color $ lighten 0.4137 $ colors.accent1
      }

warning :: forall props. PropsModifier (component :: Banner | props)
warning =
  style \(LumiTheme { colors }) ->
    css
      { backgroundColor: color $ lighten 0.4137 $ colors.accent2
      }

error :: forall props. PropsModifier (component :: Banner | props)
error =
  style \(LumiTheme { colors }) ->
    css
      { backgroundColor: color $ lighten 0.4137 $ colors.accent3
      }
