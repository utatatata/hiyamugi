module Hiyamugi.Component.HTML.Header where

import Prelude
import Data.Monoid (guard)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Hiyamugi.Component.HTML.Utils (css, safeHref)
import Hiyamugi.Data.Route (Route(..))

header :: forall i p. Route -> HH.HTML i p
header route =
  HH.nav
    [ css "navbar is-fixed-top"
    , HPA.role "navigation"
    , HPA.label "main navigation"
    ]
    [ HH.div
        [ css "navbar-brand" ]
        [ navbarItem Home
            [ HH.text "Hiyamugi" ]
        , HH.a
            [ HPA.role "button"
            , css "navbar-burger burger"
            , HPA.label "menu"
            , HPA.expanded "false"
            , HP.target "fixedNavbar"
            ]
            [ HH.span [ HPA.hidden "true" ] []
            , HH.span [ HPA.hidden "true" ] []
            ]
        ]
    , HH.div
        [ HP.id_ "fixedNavbar"
        , css "navbar-menu"
        ]
        [ HH.div
            [ css "navbar-start" ]
            [ navbarItem Home
                [ HH.text "Home" ]
            , navbarItem Importer
                [ HH.text "Importer" ]
            ]
        ]
    ]
  where
  navbarItem r html =
    HH.a
      [ css $ "navbar-item" <> guard (route == r) " is-active"
      , safeHref r
      ]
      html
