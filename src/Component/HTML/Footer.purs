module Hiyamugi.Component.HTML.Footer where

import Prelude

import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Hiyamugi.Component.HTML.Utils (css)
import Hiyamugi.Data.Route (Route)

footer :: forall i p. HH.HTML i p
footer =
  HH.nav
    [ css "navbar" ]
    [ HH.text "footer"
    ]
