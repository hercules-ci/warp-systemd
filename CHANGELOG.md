# Revision history for warp-systemd

## 0.3.0.0 -- 2023-06-22

 * By default, `runSystemdWarp` now installs a shutdown signal handler for `SIGINT` in addition to `SIGTERM`.

 * If you don't want to install a shutdown signal handler, use the new `setDontOverrideInstallShutdownHandler`.

 * The repository now comes with
     - example systemd configuration in the README
     - example NixOS service
     - a NixOS test for the watchdog feature, which is also a good example

## 0.2.0.0 -- 2021-07-06

 * Allow using healthchecks as conditional to trigger heartbeat

## 0.1.1.0 -- 2020-07-09

 * Update `network` package and use `withFdSocket` ([network#399](https://github.com/haskell/network/pull/399))

## 0.1.0.0 -- 2019-09-18

 * First version. Released on an unsuspecting world.
