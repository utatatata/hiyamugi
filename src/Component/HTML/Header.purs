module Hiyamugi.Component.HTML.Header where

import Prelude

import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Hiyamugi.Component.HTML.Utils (css)
import Hiyamugi.Data.Route (Route)

header :: forall i p. Route -> HH.HTML i p
header route =
  HH.nav
    [ css "navbar" ]
    [ HH.text "header"
    ]
