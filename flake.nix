{
  description = "Trying to build the perfect system";

  inputs = {
    # I use unstable by default
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.11-small";
    impermanence.url = "github:nix-community/impermanence";

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

    # Not on melpa yet.
    copilot-el = {
      url = "github:zerolfx/copilot.el";
      flake = false;
    };
  };

  outputs =
    { nixpkgs
    , nixpkgs-stable
    , impermanence
    , ...
    }@inputs:
    let
      inherit (nixpkgs) lib;
      inherit (lib) nixosSystem;
      inherit (lib.attrsets) genAttrs;

      system = "x86_64-linux";
      hosts = [
        "blazar"
        "ton-618"
      ];

      # see https://nixos.wiki/wiki/Flakes#Importing_packages_from_multiple_channels
      overlay-stable = _final: _prev: {
        stable = import nixpkgs-stable {
          inherit system;
          config.allowUnfree = true;
        };
      };

      toConfig = hostName:
        nixosSystem {
          inherit system;
          modules = [
            (./hosts + "/${hostName}.nix")
            impermanence.nixosModules.impermanence
            inputs.home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = { inherit inputs; };
              };
            }
            {
              networking.hostName = hostName;
              nixpkgs.overlays =
                [
                  inputs.emacs-overlay.overlays.default
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
