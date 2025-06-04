{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    git-hooks-nix.url = "github:cachix/git-hooks.nix";
    git-hooks-nix.inputs.nixpkgs.follows = "nixpkgs";
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
    hercules-ci-effects.inputs.nixpkgs.follows = "nixpkgs";
    hercules-ci-effects.inputs.flake-parts.follows = "flake-parts";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ lib, withSystem, ... }: {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.git-hooks-nix.flakeModule
        inputs.hercules-ci-effects.flakeModule
      ];

      perSystem = { config, self', pkgs, ... }: {

        checks.nixos = pkgs.testers.runNixOSTest {
          imports = [ ./example-nixos/test.nix ];
          defaults = { pkgs, ... }: {
            imports = [ ./example-nixos/warp-systemd-example.nix ];
            services.warp-systemd-example.package = withSystem pkgs.stdenv.hostPlatform.system ({ config, ... }:
              config.packages.default
            );
          };
        };

        haskellProjects."project" = {
          # all defaults
        };

        devShells.default = config.devShells.project.overrideAttrs (o: {
          shellHook = ''
            ${o.shellHook}
            ${config.pre-commit.installationScript}

            echo "Welcome to the dev shell!"
          '';
          nativeBuildInputs = o.nativeBuildInputs ++ [ pkgs.haskellPackages.releaser ];
        });

        packages.default = config.packages.project-warp-systemd;

        pre-commit.settings.hooks = {
          nixpkgs-fmt.enable = true;
          ormolu.enable = true;
        };
      };

      herculesCI.ciSystems = [ "x86_64-linux" "aarch64-darwin" "aarch64-linux" ];
      herculesCI.onPush.default.outputs.devShells.aarch64-darwin = lib.mkForce { };
      # can trip up a resource limited builder
      herculesCI.onPush.default.outputs.devShells.aarch64-linux = lib.mkForce { };
      # builder doesn't virtualize itself; rely on darwin host instead
      herculesCI.onPush.default.outputs.checks.aarch64-linux.nixos = lib.mkForce { };

      hercules-ci.flake-update = {
        enable = true;
        when = {
          dayOfMonth = [ 3 ];
        };
      };

    });
}
