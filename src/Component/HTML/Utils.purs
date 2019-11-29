module Hiyamugi.Component.HTML.Utils where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Hiyamugi.Data.Route (Route, routeCodec)
import Routing.Duplex (print)

css :: forall r i. String -> HH.IProp ( class :: String | r ) i
css = HP.class_ <<< HH.ClassName

safeHref :: forall r i. Route -> HH.IProp ( href :: String | r ) i
safeHref = HP.href <<< append "#" <<< print routeCodec

maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x

maybeElem _ _ = HH.text ""

whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""
