 Systemd integration for the Warp web server (WAI).
 
 # Features

- [Socket Activation](http://0pointer.de/blog/projects/socket-activation.html)
- [Watchdog](http://0pointer.de/blog/projects/watchdog.html)

# Usage

By default all systemd integrations are off. Tweak `SystemdSettings` to enable watchdog for example:

```haskell
import Data.Function                                 ((&))
import qualified Network.Wai.Handler.Warp         as Warp
import qualified Network.Wai.Handler.Warp.Systemd as Systemd

myWaiApp = ...

main :: IO ()
main = do
  Systemd.runSystemdWarp systemdSettings Warp.defaultSettings myWaiApp
  where
    systemdSettings = Systemd.SystemdSettings
      & setHeartbeatInterval (Just 3)
```