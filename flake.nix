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
    let system = "x86_64-linux";
    in {
      packages.${system}.iso =
        self.nixosConfigurations.iso.config.system.build.isoImage;

      nixosConfigurations = {
        betelgeuse = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./hosts/betelgeuse/configuration.nix
            inputs.home-manager.nixosModules.home-manager
            { nixpkgs.overlays = [ inputs.emacs-overlay.overlay ]; }
          ];
        };
        tau-ceti = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./hosts/tau-ceti/configuration.nix
            inputs.home-manager.nixosModules.home-manager
            { nixpkgs.overlays = [ inputs.emacs-overlay.overlay ]; }
          ];
        };

        iso = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./hosts/iso.nix
            inputs.home-manager.nixosModules.home-manager
            { nixpkgs.overlays = [ inputs.emacs-overlay.overlay ]; }
          ];
        };
      };

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
