{ pkgs, ... }:
let planck = pkgs.callPackage ../../../keyboard-firmware/planck { };
in
{
  home.packages = with pkgs; [
    # dev
    bfg-repo-cleaner
    dotnet-sdk
    nix-prefetch-git
    omnisharp-roslyn
    rust-analyzer
    rustup
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

    # other
    exiftool
    feh
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
