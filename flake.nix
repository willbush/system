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

    # System-wide colorscheming and typography for NixOS
    stylix = {
      url = "github:danth/stylix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
      };
    };

    chemacs = {
      url = "github:plexus/chemacs2";
      flake = false;
    };

    wezterm = {
      url = "github:wez/wezterm/main?dir=nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # helix = {
    #   url = "github:willbush/helix/wills-mods";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
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
            inputs.impermanence.nixosModules.impermanence
            inputs.home-manager.nixosModules.home-manager
            inputs.stylix.nixosModules.stylix
            {
              home-manager = {
                backupFileExtension = "backup";
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
