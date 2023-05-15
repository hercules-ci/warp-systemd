{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.inputs.nixpkgs-stable.follows = "nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.pre-commit-hooks.flakeModule
      ];
      flake.herculesCI.ciSystems = [ "x86_64-linux" "aarch64-darwin" ];

      perSystem = { config, self', pkgs, ... }: {

        checks.nixos = pkgs.testers.runNixOSTest {
          imports = [ ./example-nixos/test.nix ];
          defaults = {
            imports = [ ./example-nixos/warp-systemd-example.nix ];
            services.warp-systemd-example.package = config.packages.default;
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
    };
}
