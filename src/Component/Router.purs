module Hiyamugi.Component.Router where

import Prelude
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Hiyamugi.Capability.Navigate (class Navigate, navigate)
import Hiyamugi.Component.Utils (OpaqueSlot)
import Hiyamugi.Data.Route (Route(..), routeCodec)
import Hiyamugi.Page.Home as Home
import Hiyamugi.Page.Importer as Importer
import Hiyamugi.Page.Player as Player
import Routing.Duplex as RD
import Routing.Hash (getHash)

type State
  = { route :: Maybe Route
    }

data Query a
  = Navigate Route a

data Action
  = Initialize

type ChildSlots
  = ( home :: OpaqueSlot Unit
    , player :: OpaqueSlot Unit
    , importer :: OpaqueSlot Unit
    )

component ::
  forall m.
  MonadAff m =>
  Navigate m =>
  H.Component HH.HTML Query {} Void m
component =
  H.mkComponent
    { initialState: \_ -> { route: Nothing }
    , render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleQuery = handleQuery
            , handleAction = handleAction
            , initialize = Just Initialize
            }
    }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
      navigate $ fromMaybe Home initialRoute

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      H.modify_ _ { route = Just dest }
      pure $ Just a

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = case route of
    Just r -> case r of
      Home -> HH.slot (SProxy :: _ "home") unit Home.component {} absurd
      Importer -> HH.slot (SProxy :: _ "importer") unit Importer.component {} absurd
      Player -> HH.slot (SProxy :: _ "player") unit Player.component {} absurd
    Nothing -> HH.div_ [ HH.text "That page wasn't found." ]
