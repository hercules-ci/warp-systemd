/*

  A NixOS VM test that tests the watchdog functionality.

  nix build .#checks.x86_64-linux.nixos -L

*/
{ ... }: {
  name = "warp-systemd-example";

  nodes.server = {
    config = {
      services.warp-systemd-example.enable = true;
      services.warp-systemd-example.openFirewall = true;
    };
  };

  nodes.client = { };

  testScript = ''
    start_all()
    server.wait_for_unit('warp-systemd-example.socket')
    server.wait_for_unit('network.target')
    client.wait_for_unit('network.target')

    # let's go

    client.succeed('curl --fail http://server | grep "Hello World"')

    # all good, now let's pause the service and trigger the watchdog

    server.succeed('exec 1>&2; echo pid:; pidof warp-systemd-example')
    server.succeed('exec 1>&2; echo pausing service...; kill -s STOP $(pidof warp-systemd-example); echo waiting for watchdog to recover the service...; sleep 20')

    # did we recover?

    client.succeed('curl --fail http://server | grep "Hello World"')

    # did we get a coredump?

    server.succeed('coredumpctl list | grep warp-systemd-example')

  '';
}
