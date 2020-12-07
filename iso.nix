{ pkgs, ... }:

{
  users.users.nixos.shell = pkgs.zsh;

  environment.systemPackages = with pkgs; [ git ripgrep tree wget ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;

    users.nixos = { pkgs, ... }: {
      imports = [
        (import ./emacs.nix {
          inherit pkgs;
          emacsPackage = pkgs.emacsGit;
        })
      ];

      programs.starship.enable = true;

      programs.zsh = {
        enable = true;
        enableCompletion = true;
        enableAutosuggestions = true;
      };
    };
  };
}
