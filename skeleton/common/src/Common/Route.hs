{-# LANGUAGE CPP #-}
module Common.Route where

import Control.Monad.Except (MonadError, throwError)
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Signed (Signed(..))
import Obelisk.Route
import Obelisk.Route.TH

#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
import Database.Beam.Schema (PrimaryKey)
import Rhyolite.Account (Account)

-- | Beam primary key alias (native builds only).
type Id a = PrimaryKey a Identity
-- | Signed account token used in auth routes.
type SignedAccountToken = Signed (Id Account)
#else
-- | Phantom type standing in for the account key on WASM/JS frontends.
-- Signed's type parameter is phantom — the URL encoding is just Text.
data AccountId
type SignedAccountToken = Signed AccountId
#endif

-- | API sub-routes under @/api/@.
data ApiRoute :: Type -> Type where
  ApiRoute_Login :: ApiRoute (Maybe SignedAccountToken)
  ApiRoute_ResetPassword :: ApiRoute (Maybe SignedAccountToken)
  ApiRoute_Email :: ApiRoute ()

data BackendRoute :: Type -> Type where
  -- | Root landing page, served as static HTML.
  BackendRoute_Landing :: BackendRoute ()
  -- | About page.
  BackendRoute_About :: BackendRoute ()
  -- | Blog index page.
  BackendRoute_Blog :: BackendRoute ()
  -- | robots.txt
  BackendRoute_RobotsTxt :: BackendRoute ()
  -- | Rhyolite listen endpoint for push notifications.
  BackendRoute_Listen :: BackendRoute ()
  -- | All API endpoints live under @/api/@.
  BackendRoute_Api :: BackendRoute (R ApiRoute)

data FrontendRoute :: Type -> Type where
  -- | Main authenticated app page.
  FrontendRoute_Main :: FrontendRoute ()
  -- | Login page.
  FrontendRoute_Login :: FrontendRoute ()
  -- | Signup page.
  FrontendRoute_Signup :: FrontendRoute ()
  -- | Password reset page: optional signed account id token.
  FrontendRoute_ResetPassword :: FrontendRoute (Maybe SignedAccountToken)
  -- | Request password reset page.
  FrontendRoute_RequestNewPassword :: FrontendRoute ()



concat <$> mapM deriveRouteComponent
  [ ''ApiRoute
  , ''BackendRoute
  , ''FrontendRoute
  ]



-- | Encode 'Signed a' as a single URL path segment (the signed text payload).
signedEncoder :: (Applicative check, MonadError Text parse)
  => Encoder check parse (Signed a) PageName
signedEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = \(Signed t) -> ([t], mempty)
  , _encoderImpl_decode = \(p, _q) -> case p of
      [t] -> pure (Signed t)
      _ -> throwError "signedEncoder: expected exactly one path segment"
  }

-- | Encode 'Maybe (Signed a)' — Nothing ≡ end of path, Just ≡ one more segment.
maybeSignedEncoder :: (Applicative check, MonadError Text check)
  => Encoder check check (Maybe (Signed a)) PageName
maybeSignedEncoder = maybeEncoder (unitEncoder mempty) signedEncoder

-- | Encoder for API sub-routes.
apiRouteEncoder
  :: (MonadError Text check)
  => ApiRoute a -> SegmentResult check check a
apiRouteEncoder = \case
  ApiRoute_Login -> PathSegment "login" maybeSignedEncoder
  ApiRoute_ResetPassword -> PathSegment "reset-password" maybeSignedEncoder
  ApiRoute_Email -> PathSegment "email" $ unitEncoder mempty

checkedFullRouteEncoder :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedFullRouteEncoder = case checkEncoder fullRouteEncoder of
  Left e -> error $ show e
  Right x -> x

-- | The default/unrecognized route falls through to the landing page.
fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Landing :/ ())
  (\case
    BackendRoute_Landing -> PathEnd $ unitEncoder mempty
    BackendRoute_About -> PathSegment "about" $ unitEncoder mempty
    BackendRoute_Blog -> PathSegment "blog" $ unitEncoder mempty
    BackendRoute_RobotsTxt -> PathSegment "robots.txt" $ unitEncoder mempty
    BackendRoute_Listen -> PathSegment "listen" $ unitEncoder mempty
    BackendRoute_Api -> PathSegment "api" $ pathComponentEncoder apiRouteEncoder
  )
  (\case
    FrontendRoute_Main -> PathSegment "app" $ unitEncoder mempty
    FrontendRoute_Login -> PathSegment "login" $ unitEncoder mempty
    FrontendRoute_Signup -> PathSegment "signup" $ unitEncoder mempty
    FrontendRoute_ResetPassword -> PathSegment "reset-password" maybeSignedEncoder
    FrontendRoute_RequestNewPassword -> PathSegment "request-new-password" $ unitEncoder mempty
  )
