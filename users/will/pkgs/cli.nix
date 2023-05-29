{ pkgs, ... }:
let
  planck = pkgs.callPackage ../../../keyboard-firmware/planck { };
  crkbd = pkgs.callPackage ../../../keyboard-firmware/crkbd { };
in
{
  home.packages = with pkgs; [
    # dev
    azure-cli
    bfg-repo-cleaner
    gdb
    git-filter-repo
    nix-prefetch-git
    nodejs # for copilot.el login
    omnisharp-roslyn
    rnix-lsp
    sqlfluff
    tokei
    vault

    # k8s
    argocd
    kubectl
    kubernetes-helm
    minikube
    octant

    # tui
    nethogs

    # pandoc related
    pandoc
    texlive.combined.scheme-small # things needed for pandoc

    # network
    dnsutils
    openconnect
    rustscan
    xh

    # Keyboard firmware flash
    crkbd
    planck

    # other
    exiftool
    feh
    ffmpeg
    gource # git history visualization
    lsof
    nushell # command is nu
    tesseract5
    xclip
    xdotool
    zola
  ];
}
