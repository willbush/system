{ pkgs, ... }:
{
  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    alacritty.enable = true;
    bat.enable = true;
    bottom.enable = true; # command: btm
    btop.enable = true;
    htop.enable = true;
    imv.enable = true; # command line image viewer intended for use with tiling window managers.
    rbw = {
      enable = true;
      # TODO: remove after https://github.com/doy/rbw/issues/218 is merged
      package = pkgs.rbw.overrideAttrs (_: {
        src = pkgs.fetchFromGitHub {
          owner = "davla";
          repo = "rbw";
          rev = "fix/client-name-header";
          sha256 = "sha256-Sgs+qjKdtS5i7zF86TLSZMVKTDoeYhIgKEwjUUXw/cc=";
        };
        cargoDeps = pkgs.rustPlatform.importCargoLock {
          lockFile = (
            pkgs.fetchurl {
              url = "https://raw.githubusercontent.com/davla/rbw/dd6b65427de3a4b38d27350d8ad7ebacb29e97ff/Cargo.lock";
              hash = "sha256-bAELLBb0x0BOGPMLBRX/s0qxqN8XOxUW9OUa55WjeaA=";
            }
          );
          allowBuiltinFetchGit = true;
        };
      });
    };
    skim.enable = true;
    wofi.enable = true;
    yazi.enable = true;
    zoxide.enable = true;
  };
}
