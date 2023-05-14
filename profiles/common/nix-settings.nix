{ pkgs, ... }: {
  nix = {
    # Required until nix version 2.4 for nix flakes
    package = pkgs.nixUnstable;

    # The keep-* options below protect the nix-shell against garbage collection
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';

    settings = {
      auto-optimise-store = true;
      substituters = [
        "https://cache.nixos.org/"
        "https://hercules-ci.cachix.org"
        "https://nix-community.cachix.org"
        "https://nix-tools.cachix.org"
      ];
      trusted-public-keys = [
        "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nix-tools.cachix.org-1:ebBEBZLogLxcCvipq2MTvuHlP7ZRdkazFSQsbs0Px1A="
      ];
    };
    gc = {
      automatic = true;
      dates = "23:00";
      options = "--delete-older-than 30d";
    };
  };
}
