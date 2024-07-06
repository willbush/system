{
  description = "Trying to build the perfect system";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
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

    # Never going to be on melpa.
    copilot-el = {
      url = "github:zerolfx/copilot.el";
      flake = false;
    };
    # Eventually will be on elpa https://github.com/nemethf/eglot-x/issues/1
    eglot-x = {
      url = "github:nemethf/eglot-x";
      flake = false;
    };
    atomic-chrome = {
      url = "github:KarimAziev/atomic-chrome";
      flake = false;
    };
    # Soothing pastel theme for Nix
    catppuccin.url = "github:catppuccin/nix";
  };

  outputs =
    {
      catppuccin,
      impermanence,
      nixpkgs,
      ...
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

      toConfig =
        hostName:
        nixosSystem {
          inherit system;
          modules = [
            (./hosts + "/${hostName}.nix")
            catppuccin.nixosModules.catppuccin
            impermanence.nixosModules.impermanence
            inputs.home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = {
                  inherit inputs;
                };
              };
            }
            {
              networking.hostName = hostName;
              nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ];
            }
          ];
          specialArgs = {
            inherit inputs;
          };
        };
    in
    {
      nixosConfigurations = genAttrs hosts toConfig;
    };
}
