{
  description = "The system configuration of a professional yak shaver";

  inputs = {
    # I use unstable by default
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.05-small";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    chemacs = {
      url = "github:plexus/chemacs2";
      flake = false;
    };
    crafted-emacs = {
      url = "github:SystemCrafters/crafted-emacs";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";

    # Not on melpa yet.
    copilot-el = {
      url = "github:zerolfx/copilot.el";
      flake = false;
    };
    chatgpt-shell = {
      url = "github:xenodium/chatgpt-shell";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgs-stable, ... }@inputs:
    let
      inherit (nixpkgs) lib;
      inherit (lib) nixosSystem;
      inherit (lib.attrsets) genAttrs;

      system = "x86_64-linux";
      hosts = [
        "bellatrix"
        "betelgeuse"
        "iso"
        "mintaka"
        "mira"
        "nixos-wsl"
        "tau-ceti"
        "ton-618"
      ];

      # see https://nixos.wiki/wiki/Flakes#Importing_packages_from_multiple_channels
      overlay-stable = final: prev: {
        stable = import nixpkgs-stable {
          inherit system;
          config.allowUnfree = true;
        };
        # use this variant if unfree packages are not needed:
        # stable = nixpkgs-stable.legacyPackages.${prev.system};
      };

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
            {
              nixpkgs.overlays =
                [
                  inputs.emacs-overlay.overlay
                  # Make "pkgs.stable" available
                  overlay-stable
                ];
            }
          ];
          specialArgs = { inherit inputs; };
        };
    in
    {
      packages.${system}.iso =
        self.nixosConfigurations.iso.config.system.build.isoImage;

      nixosConfigurations = genAttrs hosts toConfig;

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
