{-# LANGUAGE OverloadedStrings #-}

import Data.Function ((&))
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Systemd as Systemd

app :: Application
app _ respond =
  respond $ responseLBS status200 [] "Hello World"

main :: IO ()
main =
  Systemd.runSystemdWarp systemdSettings Warp.defaultSettings app
  where
    systemdSettings =
      Systemd.defaultSystemdSettings
        & Systemd.setHeartbeatInterval (Just 5)
