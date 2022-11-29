{ pkgs, ... }:
let planck = pkgs.callPackage ../../../keyboard-firmware/planck { };
in
{
  home.packages = with pkgs; [
    # dev
    azure-cli
    bfg-repo-cleaner
    jdk
    nix-prefetch-git
    omnisharp-roslyn
    powershell
    rnix-lsp
    rust-analyzer
    tokei

    # k8s
    argocd
    kubectl
    kubernetes-helm
    lens
    minikube

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

    # other
    exiftool
    feh
    ffmpeg
    gource # git history visualization
    lsof
    neofetch
    niv
    nushell
    planck
    woeusb # Windows ISO to USB drive utility
    xclip
    xdotool
    zola
  ];
}
