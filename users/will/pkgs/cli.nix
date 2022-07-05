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
    kubectl
    kubernetes-helm
    minikube
    argocd

    # tui
    nethogs

    # pandoc related
    pandoc
    texlive.combined.scheme-small # things needed for pandoc

    # other
    dnsutils
    exiftool
    feh
    lsof
    neofetch
    niv
    openconnect
    planck
    woeusb # Windows ISO to USB drive utility
    xclip
    xdotool
    zola
  ];
}
