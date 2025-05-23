{ pkgs, ... }:
{
  nix = {
    package = pkgs.nixVersions.latest;

    # Automatically run the nix store optimiser at a specific time. "If the
    # system is off during the expected execution time, the timer is executed
    # once the system is running again." The other option `auto-optimise-store =
    # true` runs optimise on every build, which in theory has some overhead.
    optimise.automatic = true;

    settings = {
      trusted-users = [
        "root"
        "@wheel"
      ];
      experimental-features = "nix-command flakes";
      substituters = [
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };
}
