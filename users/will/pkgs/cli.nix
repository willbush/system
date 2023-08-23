{ config, pkgs, ... }:
let
  planck = pkgs.callPackage ../../../keyboard-firmware/planck { };
  crkbd = pkgs.callPackage ../../../keyboard-firmware/crkbd { };
  kubectl-vsphere = pkgs.callPackage ../kubectl-vsphere.nix {
    syncthingEnabled = config.modules.services.syncthing.enable;
  };
in
{
  home.packages = with pkgs; [
    # dev
    gdb
    nodejs # for copilot.el login
    omnisharp-roslyn # broken until https://github.com/NixOS/nixpkgs/pull/249091
    sqlfluff
    tokei
    vault

    # k8s
    argocd
    k9s
    kube-score
    kubectl
    kubectl-vsphere
    kubernetes-helm
    kubeval
    kustomize
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
    feh
    lsof
    tesseract5
    xclip
    xdotool
    zola
  ];
}
