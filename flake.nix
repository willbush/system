{
  description = "Trying to build the perfect system";

  inputs = {
    impermanence.url = "github:nix-community/impermanence";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };

    helix = {
      url = "github:willbush/helix/colemak-dhm";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixgl = {
      url = "github:nix-community/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "helix/flake-utils";
    };

    # System-wide colorscheming and typography for NixOS
    stylix = {
      url = "github:danth/stylix";
      inputs = {
        flake-utils.follows = "helix/flake-utils";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };

    chemacs = {
      url = "github:plexus/chemacs2";
      flake = false;
    };

    lianli-pwm-rgb-sync = {
      url = "github:willbush/lianli-pwm-rgb-sync";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        crane.follows = "helix/crane";
        flake-utils.follows = "helix/flake-utils";
      };
    };

    zjstatus = {
      url = "github:dj95/zjstatus";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "helix/flake-utils";
        crane.follows = "helix/crane";
        rust-overlay.follows = "helix/rust-overlay";
      };
    };
  };

  outputs =
    { nixpkgs, ... }@inputs:
    let
      inherit (nixpkgs) lib;
      inherit (lib) nixosSystem;
      inherit (lib.attrsets) genAttrs;

      system = "x86_64-linux";
      hosts = [
        "blazar"
      ];

      toConfig =
        hostName:
        nixosSystem {
          inherit system;
          modules = [
            (./hosts + "/${hostName}.nix")
            inputs.home-manager.nixosModules.home-manager
            inputs.impermanence.nixosModules.impermanence
            inputs.sops-nix.nixosModules.sops
            inputs.stylix.nixosModules.stylix
            {
              home-manager = {
                backupFileExtension = "backup";
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = {
                  inherit inputs;
                };
                sharedModules = [
                  inputs.sops-nix.homeManagerModules.sops
                ];
              };
            }
            {
              networking.hostName = hostName;
              nixpkgs.overlays = [
                inputs.emacs-overlay.overlays.default
                inputs.nixgl.overlay
              ];
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
