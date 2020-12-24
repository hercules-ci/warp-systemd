{-# LANGUAGE RankNTypes #-}
-- | This modules provides function that help start the Warp web
-- server using systemd's socket activation feature.
module Network.Wai.Handler.Warp.Systemd
  ( runSystemdWarp
    -- * Settings
  , SystemdSettings
  , defaultSystemdSettings

  , logInfo
  , setLogInfo

  , logWarn
  , setLogWarn

  , requireSocketActivation
  , setRequireSocketActivation

  , heartbeatInterval
  , setHeartbeatInterval

  , heartbeatCheck
  , setHeartbeatCheck

  , onBeginShutdown
  , setOnBeginShutdown

    -- * Low-level Settings
  , dontOverrideInstallShutdownHandler, setDontOverrideInstallShutdownHandler
    -- * Exceptions
  , SocketActivationException(..)
  ) where

import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Exception
import           Control.Monad
import           Data.Function
import           Data.Typeable
import           Network.Socket           (withFdSocket, setNonBlockIfNeeded)
import           Network.Wai              as Wai
import           Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Internal as WarpInternal
import qualified System.Systemd.Daemon    as Systemd
import qualified System.IO as SIO
import qualified System.Posix.Signals as Signals

-- | These only occur during startup.
data SocketActivationException = SocketActivationException String
  deriving (Show, Typeable)

instance Exception SocketActivationException

-- | Warp-systemd integration settings. See the lenses in this module for details.
--
-- Note that Warp itself has some settings related to the server process lifecycle, for example 'Warp.setInstallShutdownHandler'.

data SystemdSettings =
  SystemdSettings
  { _logInfo :: String -> IO ()
  , _logWarn :: String -> IO ()
  , _requireSocketActivation :: Bool
  , _heartbeatInterval :: Maybe Int
  , _heartbeatCheck :: IO ()
  , _dontOverrideInstallShutdownHandler :: Bool
  , _onBeginShutdown :: IO ()
  }

-- | Default settings. See the lenses in this module for details.
defaultSystemdSettings :: SystemdSettings
defaultSystemdSettings = SystemdSettings
  { _logInfo = SIO.hPutStrLn SIO.stderr
  , _logWarn = SIO.hPutStrLn SIO.stderr . ("WARNING: " ++)
  , _requireSocketActivation = False
  , _heartbeatInterval = Nothing
  , _heartbeatCheck = return ()
  , _dontOverrideInstallShutdownHandler = False
  , _onBeginShutdown = return ()
  }

-- | How to log an info message.
--
-- Default: @hPutStrLn stderr@
logInfo :: Lens' SystemdSettings (String -> IO ())
logInfo = lens _logInfo setLogInfo

-- | How to log an info message.
--
-- Default: @hPutStrLn stderr . ("WARNING: " ++)@
logWarn :: Lens' SystemdSettings (String -> IO ())
logWarn = lens _logWarn setLogWarn

-- | If True, 'runSystemdWarp' throw a 'SocketActivationException' if
-- the server is started without socket activation.
--
-- Default: @False (continue)@
requireSocketActivation :: Lens' SystemdSettings Bool
requireSocketActivation = lens _requireSocketActivation setRequireSocketActivation

-- | If @Just n@, 'runSystemdWarp' emits a heartbeat notification to
-- systemd every @n@ seconds.
--
-- Default: @Nothing@
heartbeatInterval :: Lens' SystemdSettings (Maybe Int)
heartbeatInterval = lens _heartbeatInterval setHeartbeatInterval

-- | Run an action before emitting a hearbeat and if it throws an exception, print a warning
--   and skip systemd notification.
--
-- Default: @return ()@
heartbeatCheck :: Lens' SystemdSettings (IO ())
heartbeatCheck = lens _heartbeatCheck setHeartbeatCheck

-- | If @True@, do not override 'Warp.Settings'' with
-- 'setInstallShutdownHandler'. This lets you provide your own
-- shutdown handler functionality. Enabling this setting will
-- cause the default 'installShutdownHandler' to not be set,
-- with the effect of preventing the 'onBeginShutdown' action and
-- preventing the systemd ‘stopping’ notification.
-- 
--
-- Default: @Nothing@
dontOverrideInstallShutdownHandler :: Lens' SystemdSettings Bool
dontOverrideInstallShutdownHandler = lens _dontOverrideInstallShutdownHandler setDontOverrideInstallShutdownHandler

-- | Action to run on shutdown. This will be called when a shutdown
-- signal has been received from systemd and the listening socket has
-- been closed. This means that no new incoming requests will be
-- received, but previous requests are still being processed.
--
-- Control flow should return to the caller of 'runSystemdWarp' when
-- all requests have been handled.
--
-- Default: 'return ()'
onBeginShutdown :: Lens' SystemdSettings (IO ())
onBeginShutdown = lens _onBeginShutdown setOnBeginShutdown

-- | See 'logInfo'
setLogInfo :: (String -> IO ()) -> SystemdSettings -> SystemdSettings
setLogInfo x s = s { _logInfo = x }

-- | See 'logWarn'
setLogWarn :: (String -> IO ()) -> SystemdSettings -> SystemdSettings
setLogWarn x s = s { _logWarn = x }

-- | See 'requireSocketActivation'
setRequireSocketActivation :: Bool -> SystemdSettings -> SystemdSettings
setRequireSocketActivation x s = s { _requireSocketActivation = x }

-- | See 'heartbeatInterval'
setHeartbeatInterval :: Maybe Int -> SystemdSettings -> SystemdSettings
setHeartbeatInterval x s = s { _heartbeatInterval = x }

-- | See 'heartbeatCheck'
setHeartbeatCheck :: IO () -> SystemdSettings -> SystemdSettings
setHeartbeatCheck action s = s { _heartbeatCheck = action }

-- | See 'dontOverrideInstallShutdownHandler'
setDontOverrideInstallShutdownHandler :: Bool -> SystemdSettings -> SystemdSettings
setDontOverrideInstallShutdownHandler x s = s { _dontOverrideInstallShutdownHandler = x }

-- | See 'onBeginShutdown'
setOnBeginShutdown :: IO () -> SystemdSettings -> SystemdSettings
setOnBeginShutdown x s = s { _onBeginShutdown = x }




-- | Run a web application, see 'SystemdSettings' for details.
--
-- Note that Warp itself has some 'Warp.Settings' settings related to
-- the server process lifecycle, such as
-- 'Warp.setInstallShutdownHandler'. However, you do not have to
-- include a ready notification using 'Warp.setBeforeMainloop', because
-- 'runSystemdWarp' does this for you.
runSystemdWarp
  :: SystemdSettings
  -> Warp.Settings     -- ^ Web server settings
  -> Wai.Application   -- ^ Web application
  -> IO ()
runSystemdWarp saSettings settings app = do

  forM_ (_heartbeatInterval saSettings) $ \interval -> do
    forkIO (heartbeat (_logWarn saSettings) (_heartbeatCheck saSettings) interval)
  
  socketActivationSockets <- Systemd.getActivatedSockets


  maybeSocket <- case socketActivationSockets of
    Just [socket] -> return (Just socket)

    Nothing | _requireSocketActivation saSettings ->
      throwIO (SocketActivationException "Socket activation is required to run this web application.")
      
    Nothing ->
      return Nothing

    Just [] ->
      throwIO (SocketActivationException "Socket activation seems active, but no sockets were passed to the process.")

    Just _ ->
      {- It is not entirely obvious how this should be implemented. When
         implementing, verify and document interaction with cleanup
         actions, notifications etc.
       -}
      throwIO (SocketActivationException "Multiple sockets were passed to the process, but only one socket was expected.")

  case maybeSocket of
    Just _ -> _logInfo saSettings "Warp is socket-activated"
    Nothing ->  _logInfo saSettings "Warp is not socket-activated"

  let
    inhibitIf :: Bool -> (a -> a) -> (a -> a)
    inhibitIf False x = x
    inhibitIf True  _ = id -- inhibited: leave unaltered

    settings' = settings
                & setBeforeMainLoop (do
                     WarpInternal.settingsBeforeMainLoop settings
                     void Systemd.notifyReady
                  )
                & inhibitIf (_dontOverrideInstallShutdownHandler saSettings) (
                     setInstallShutdownHandler $ \closeListenSocket ->
                         -- Maybe append/prepend this to the old setting?
                         -- But what about multiple sockets?
                         -- No obvious semantics to implement, sadly.
                         -- If multi-socket is needed, do the research and
                         -- probably create a bunch of new settings with
                         -- compatible defaults...
                         let handler = Signals.Catch $ do
                               void Systemd.notifyStopping
                               closeListenSocket
                               _onBeginShutdown saSettings
                         in void $ Signals.installHandler Signals.sigTERM handler Nothing
                     )

  case maybeSocket of
    Just socket -> do
      withFdSocket socket $ \fd -> do
        setNonBlockIfNeeded fd
        runSettingsSocket settings' socket app
    Nothing ->
      runSettings settings' app

heartbeat :: (String -> IO ()) -> IO () -> Int -> IO ()
heartbeat flogWarn action delaySeconds = loop where
  loop = do
    let delayMicroSeconds = delaySeconds * 1000 * 1000
    eitherCheck <- try action
    case eitherCheck of
      Left exc -> do
        flogWarn $ "Systemd heartbeat check failed: " <> displayException (exc :: SomeException)
        threadDelay delayMicroSeconds
        loop
      Right () -> do
        r <- Systemd.notifyWatchdog
        case r of
          Nothing -> do
            flogWarn "Systemd heartbeat notification does not seem to arrive. Stopping heartbeat notifications."
            return ()
          Just _ -> do
            threadDelay delayMicroSeconds
            loop

---------------- Minimal dependency-free lens ----------------

-- | Traverse a single element. The essence of getting and setting.
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
-- | Monomorphic 'Lens'
type Lens' s a = Lens s s a a

lens :: (s -> a) -> (b -> s -> t) -> Lens s t a b
lens sa sbt afb s = flip sbt s <$> afb (sa s)
