{
  description = "The system configuration of a professional yak shaver";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, ... }@inputs: {
    iso = self.nixosConfigurations.iso.config.system.build.isoImage;

    nixosConfigurations = {
      betelgeuse = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/betelgeuse/configuration.nix
          inputs.home-manager.nixosModules.home-manager
          { nixpkgs.overlays = [ inputs.emacs-overlay.overlay ]; }
        ];
      };
      tau-ceti = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/tau-ceti/configuration.nix
          inputs.home-manager.nixosModules.home-manager
          { nixpkgs.overlays = [ inputs.emacs-overlay.overlay ]; }
        ];
      };
      iso = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/iso/configuration.nix
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
          inputs.home-manager.nixosModules.home-manager
          { nixpkgs.overlays = [ inputs.emacs-overlay.overlay ]; }
        ];
      };
    };
  };
}
