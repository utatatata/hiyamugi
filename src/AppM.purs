module Hiyamugi.AppM where

import Prelude
import Control.Monad.Reader (class MonadAsk, ReaderT(..), asks, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Hiyamugi.Capability.Navigate (class Navigate)
import Hiyamugi.Data.Route as Route
import Hiyamugi.Env (Env)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Type.Equality (class TypeEquals, from)

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance navigateAppM :: Navigate AppM where
  navigate = liftEffect <<< setHash <<< print Route.routeCodec
