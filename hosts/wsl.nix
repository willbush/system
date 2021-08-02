{ pkgs, ... }:
{
  imports = [
    (import ../users/profiles/emacs.nix {
      inherit pkgs;
      emacsPackage = pkgs.emacsGcc;
    })
  ];
}
