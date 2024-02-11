{
  description = "The system configuration of a professional yak shaver";

  inputs = {
    # I use unstable by default
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.11-small";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    chemacs = {
      url = "github:plexus/chemacs2";
      flake = false;
    };

    crafted-emacs = {
      url = "github:SystemCrafters/crafted-emacs";
      flake = false;
    };

    # Not on melpa yet.
    copilot-el = {
      url = "github:zerolfx/copilot.el";
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
        "blazar"
        "mintaka"
        "mira"
        "nixos-wsl"
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
      nixosConfigurations = genAttrs hosts toConfig;
    };
}
