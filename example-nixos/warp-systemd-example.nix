/*

  A NixOS module that runs the example from the ../example directory as a
  systemd system service.

*/
{ lib, config, ... }:
let
  cfg = config.services.warp-systemd-example;
in
{
  options = {
    services.warp-systemd-example = {
      enable = lib.mkEnableOption "Example service";
      port = lib.mkOption {
        type = lib.types.int;
        default = 80;
        description = "Port to listen on";
      };
      package = lib.mkOption {
        type = lib.types.package;
        # If you publish to hackage, perhaps add the default.
        # In any case, this is set in the flake and/or test.
        # default = pkgs.haskellPackages.warp-systemd-example;
        description = "Example service package to run";
      };
      openFirewall = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Open the firewall for the port";
      };
    };
  };
  config = lib.mkIf cfg.enable {

    systemd.sockets.warp-systemd-example = {
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenStream = "${toString cfg.port}";
        Accept = "no";
      };
    };

    systemd.services.warp-systemd-example = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = lib.getExe cfg.package;
        Restart = "always";
        KillSignal = "SIGINT";
        Type = "notify";
        WatchdogSec = "15";
      };
    };

    networking.firewall = lib.mkIf cfg.openFirewall {
      allowedTCPPorts = [ cfg.port ];
    };
  };
}
