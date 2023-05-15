# warp-systemd

[Systemd](https://systemd.io/) integration for the [Warp Web Server (WAI)](https://github.com/yesodweb/wai).

## Features

- [Socket Activation](http://0pointer.de/blog/projects/socket-activation.html)
- [Watchdog](http://0pointer.de/blog/projects/watchdog.html)

## Usage

All systemd integrations are off by default. For example, you can modify `SystemdSettings` to enable the watchdog:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Function                                 ((&))
import qualified Network.Wai.Handler.Warp         as Warp
import qualified Network.Wai.Handler.Warp.Systemd as Systemd
import Network.Wai
import Network.HTTP.Types.Status                     (status200)

app :: Application
app _ respond =
  respond $ responseLBS status200 [] "Hello World"

main :: IO ()
main =
  Systemd.runSystemdWarp systemdSettings Warp.defaultSettings app
  where
    systemdSettings = Systemd.defaultSystemdSettings
      & Systemd.setHeartbeatInterval (Just 5)
```

## Systemd Service Example

Let's configure your app to run as a socket-activated systemd service. We'll be using the sample app in [example/Main.hs](example/Main.hs).

1. Create a new service unit at `/etc/systemd/system/warp.service` that'll run your Haskell binary:

   ```systemd
   [Unit]
   Description=Warp
   [Service]
   Type=notify
   WatchdogSec=5
   ExecStart=path-to-my-haskell-binary
   ```

2. Create the socket unit at `/etc/systemd/system/warp.socket` that'll listen on port 80:

   ```systemd
   [Unit]
   Description=Warp
   [Socket]
   ListenStream=80
   [Install]
   WantedBy=sockets.target
   ```

3. Enable the socket unit:

   ```bash
   $ sudo systemctl enable --now warp.socket
   ```

4. Verify that the server is running:

   ```bash
   $ systemctl status warp.socket warp.service
   ● warp.service - Warp
        Loaded: loaded (/etc/systemd/system/warp.service; linked; preset: enabled)
        Active: active (running) since Mon 2023-05-15 14:57:14 UTC; 19min ago
   TriggeredBy: ● warp.socket
      Main PID: 52528 (warp-systemd-ex)
            IP: 0B in, 0B out
            IO: 0B read, 0B written
         Tasks: 1 (limit: 19081)
        Memory: 2.1M
           CPU: 503ms
        CGroup: /system.slice/warp.service
                └─52528 path-to-my-haskell-binary

   May 15 14:57:14 nixos systemd[1]: Starting Warp...
   May 15 14:57:14 nixos warp-systemd-example[52528]: Warp is socket-activated
   May 15 14:57:14 nixos systemd[1]: Started Warp.

   ● warp.socket - Warp
        Loaded: loaded (/etc/systemd/system/warp.socket; enabled; preset: enabled)
        Active: active (running) since Mon 2023-05-15 14:11:56 UTC; 1h 5min ago
      Triggers: ● warp.service
        Listen: [::]:80 (Stream)
            IP: 796B in, 690B out
            IO: 0B read, 0B written
         Tasks: 0 (limit: 19081)
        Memory: 8.0K
           CPU: 563us
        CGroup: /system.slice/warp.socket

   May 15 14:11:56 nixos systemd[1]: Listening on warp.socket.
   ```

   ```bash
   $ curl http://localhost
   Hello World⏎
   ```
