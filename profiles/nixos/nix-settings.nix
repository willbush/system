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
        "https://codex-cli.cachix.org"
        "https://claude-code.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "codex-cli.cachix.org-1:1Br3H1hHoRYG22n//cGKJOk3cQXgYobUel6O8DgSing="
        "claude-code.cachix.org-1:YeXf2aNu7UTX8Vwrze0za1WEDS+4DuI2kVeWEE4fsRk="
      ];
    };
  };
}
