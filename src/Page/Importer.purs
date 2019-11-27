module Hiyamugi.Page.Importer where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Hiyamugi.Capability.Navigate (class Navigate)
import Hiyamugi.Component.HTML.Footer (footer)
import Hiyamugi.Component.HTML.Header (header)
import Hiyamugi.Component.HTML.Utils (css)
import Hiyamugi.Data.Route (Route(..))

data Action
  = Initialize

type State =
  {}

component
  :: forall m r
  . MonadAff m
  => Navigate m
  => H.Component HH.HTML (Const Void) {} Void m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    }
  }
  where
  initialState _ =
    {}

  handleAction :: Action -> H.HalogenM State Action () Void m Unit
  handleAction = case _ of
    Initialize -> do
      pure unit

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ header Importer
      , mainView state
      , footer
      ]

  mainView :: forall props. State -> HH.HTML props Action
  mainView state =
    HH.div
      [ css "" ]
      [ HH.text "importer"
      ]
