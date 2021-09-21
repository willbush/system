{
  description = "The system configuration of a professional yak shaver";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      inherit (nixpkgs) lib;
      inherit (lib) nixosSystem;
      inherit (lib.attrsets) genAttrs;

      system = "x86_64-linux";
      hosts = [ "betelgeuse" "tau-ceti" "mira" "bellatrix" "iso" ];

      toConfig = hostName:
        nixosSystem {
          inherit system;
          modules = [
            (./hosts + "/${hostName}.nix")
            inputs.home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
              };
              networking.hostName = hostName;
            }
            { nixpkgs.overlays = [ inputs.emacs-overlay.overlay ]; }
          ];
        };
    in {
      packages.${system}.iso =
        self.nixosConfigurations.iso.config.system.build.isoImage;

      nixosConfigurations = genAttrs hosts toConfig;

      homeConfigurations = {
        wsl = inputs.home-manager.lib.homeManagerConfiguration {
          system = "x86_64-linux";
          homeDirectory = "/home/will";
          username = "will";

          # configuration = ./home/hosts/wsl.nix;
          configuration = {
            imports = [ ./hosts/wsl.nix ];

            nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];
            # xdg.configFile."nix/nix.conf".text = ''
            #     experimental-features = nix-command flakes
            #     substituters = https://cache.nixos.org https://nix-community.cachix.org
            #     trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=
            # '';
          };
        };
      };

      # activate home-manager configuration with `nix run .#wsl`
      wsl = self.homeConfigurations.wsl.activationPackage;

      # This is a work around for `nix repl` not yet supporting flakes.
      # You can enter a repl for this flake be doing: nix run '.#repl'
      # see: https://github.com/NixOS/nix/issues/3803#issuecomment-748612294
      apps.${system}.repl = inputs.flake-utils.lib.mkApp {
        drv = nixpkgs.legacyPackages.${system}.writeShellScriptBin "repl" ''
          confnix=$(mktemp)
          echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
          trap "rm $confnix" EXIT
          nix repl $confnix
        '';
      };
    };
}
