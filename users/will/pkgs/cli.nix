{ config, pkgs, ... }:
let
  planck = pkgs.callPackage ../../../keyboard-firmware/planck { };
  crkbd = pkgs.callPackage ../../../keyboard-firmware/crkbd { };
  eksctl-anywhere = pkgs.callPackage ../eksctl-anywhere.nix { };
  kubectl-vsphere = pkgs.callPackage ../kubectl-vsphere.nix {
    syncthingEnabled = true;
  };
in
{
  home.packages = with pkgs; [
    # dev
    gdb
    nodejs # for copilot.el login
    omnisharp-roslyn # broken until https://github.com/NixOS/nixpkgs/pull/249091
    openssl
    sqlfluff
    tokei
    vault

    # k8s
    argocd
    eksctl
    eksctl-anywhere
    k9s
    kube-score
    kubeconform
    kubectl
    kubectl-vsphere
    kubernetes-helm
    kustomize
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
