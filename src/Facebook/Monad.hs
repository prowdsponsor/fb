{-# LANGUAGE CPP,
             DeriveDataTypeable,
             FlexibleContexts,
             FlexibleInstances,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             StandaloneDeriving,
             TypeFamilies,
             UndecidableInstances #-}
module Facebook.Monad
    ( FacebookT
    , Auth
    , NoAuth
    , FbTier(..)
    , runFacebookT
    , runNoAuthFacebookT
    , beta_runFacebookT
    , beta_runNoAuthFacebookT
    , getCreds
    , getManager
    , getTier
    , withTier
    , runResourceInFb
    , mapFacebookT

      -- * Re-export
    , lift
    ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, liftM)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control ( MonadTransControl(..), MonadBaseControl(..)
                                   , ComposeSt, defaultLiftBaseWith
                                   , defaultRestoreM )
#if MIN_VERSION_monad_control(1,0,0)
import Control.Monad.Trans.Control (defaultLiftWith, defaultRestoreT)
#endif
import Control.Monad.Trans.Reader (ReaderT(..), ask, mapReaderT)
import Data.Typeable (Typeable)
import qualified Control.Monad.Trans.Resource as R

import qualified Network.HTTP.Conduit as H

import Facebook.Types


-- | @FacebookT auth m a@ is this library's monad transformer.
-- Contains information needed to issue commands and queries to
-- Facebook.  The phantom type @auth@ may be either 'Auth' (you
-- have supplied your 'Credentials') or 'NoAuth' (you have not
-- supplied any 'Credentials').
newtype FacebookT auth m a = F { unF :: ReaderT FbData m a }
    deriving ( Functor, Applicative, Alternative, Monad, MonadFix
             , MonadPlus, MonadIO, MonadTrans, R.MonadThrow )

deriving instance (R.MonadResource m, MonadBase IO m) => R.MonadResource (FacebookT auth m)

instance MonadBase b m => MonadBase b (FacebookT auth m) where
    liftBase = lift . liftBase

instance MonadTransControl (FacebookT auth) where
#if MIN_VERSION_monad_control(1,0,0)
    type StT (FacebookT auth) a = StT (ReaderT FbData) a
    liftWith = defaultLiftWith F unF
    restoreT = defaultRestoreT F
#else
    newtype StT (FacebookT auth) a = FbStT { unFbStT :: StT (ReaderT FbData) a }
    liftWith f = F $ liftWith (\run -> f (liftM FbStT . run . unF))
    restoreT   = F . restoreT . liftM unFbStT
#endif

instance MonadBaseControl b m => MonadBaseControl b (FacebookT auth m) where
#if MIN_VERSION_monad_control(1,0,0)
    type StM (FacebookT auth m) a = ComposeSt (FacebookT auth) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
#else
    newtype StM (FacebookT auth m) a = StMT {unStMT :: ComposeSt (FacebookT auth) m a}
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM     = defaultRestoreM   unStMT
#endif

-- | Since @fb-0.14.8@.
instance MonadLogger m => MonadLogger (FacebookT auth m) where
    monadLoggerLog loc src lvl msg = lift (monadLoggerLog loc src lvl msg)


-- | Phantom type stating that you have provided your
-- 'Credentials' and thus have access to the whole API.
data Auth deriving (Typeable)

-- | Phantom type stating that you have /not/ provided your
-- 'Credentials'.  This means that you'll be limited about which
-- APIs you'll be able use.
data NoAuth deriving (Typeable)

-- | Internal data kept inside 'FacebookT'.
data FbData = FbData { fbdCreds   :: Credentials -- ^ Can be 'undefined'!
                     , fbdManager :: !H.Manager
                     , fbdTier    :: !FbTier
                     }
              deriving (Typeable)

-- | Which Facebook tier should be used (see
-- <https://developers.facebook.com/support/beta-tier/>).
data FbTier = Production | Beta deriving (Eq, Ord, Show, Read, Enum, Typeable)


-- | Run a computation in the 'FacebookT' monad transformer with
-- your credentials.
runFacebookT :: Credentials         -- ^ Your app's credentials.
             -> H.Manager           -- ^ Connection manager (see 'H.withManager').
             -> FacebookT Auth m a
             -> m a
runFacebookT creds manager (F act) =
    runReaderT act (FbData creds manager Production)

-- | Run a computation in the 'FacebookT' monad without
-- credentials.
runNoAuthFacebookT :: H.Manager -> FacebookT NoAuth m a -> m a
runNoAuthFacebookT manager (F act) =
    let creds = error "runNoAuthFacebookT: never here, serious bug"
    in runReaderT act (FbData creds manager Production)

-- | Same as 'runFacebookT', but uses Facebook's beta tier (see
-- <https://developers.facebook.com/support/beta-tier/>).
beta_runFacebookT :: Credentials -> H.Manager -> FacebookT Auth m a -> m a
beta_runFacebookT creds manager (F act) =
    runReaderT act (FbData creds manager Beta)

-- | Same as 'runNoAuthFacebookT', but uses Facebook's beta tier
-- (see <https://developers.facebook.com/support/beta-tier/>).
beta_runNoAuthFacebookT :: H.Manager -> FacebookT NoAuth m a -> m a
beta_runNoAuthFacebookT manager (F act) =
    let creds = error "beta_runNoAuthFacebookT: never here, serious bug"
    in runReaderT act (FbData creds manager Beta)


-- | Get the user's credentials.
getCreds :: Monad m => FacebookT Auth m Credentials
getCreds = fbdCreds `liftM` F ask

-- | Get the 'H.Manager'.
getManager :: Monad m => FacebookT anyAuth m H.Manager
getManager = fbdManager `liftM` F ask

-- | Get the 'FbTier'.
getTier :: Monad m => FacebookT anyAuth m FbTier
getTier = fbdTier `liftM` F ask

-- | Run a pure function that depends on the 'FbTier' being used.
withTier :: Monad m => (FbTier -> a) -> FacebookT anyAuth m a
withTier = flip liftM getTier


-- | Run a 'ResourceT' inside a 'FacebookT'.
runResourceInFb :: (R.MonadResource m, MonadBaseControl IO m) =>
                   FacebookT anyAuth (R.ResourceT m) a
                -> FacebookT anyAuth m a
runResourceInFb (F inner) = F $ ask >>= lift . R.runResourceT . runReaderT inner


-- | Transform the computation inside a 'FacebookT'.
mapFacebookT :: (m a -> n b) -> FacebookT anyAuth m a -> FacebookT anyAuth n b
mapFacebookT f = F . mapReaderT f . unF
