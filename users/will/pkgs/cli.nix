{ config, pkgs, ... }:
let
  planck = pkgs.callPackage ../../../keyboard-firmware/planck { };
  crkbd = pkgs.callPackage ../../../keyboard-firmware/crkbd { };
  eksctl-anywhere = pkgs.callPackage ../eksctl-anywhere.nix { };
in
{
  home.packages = with pkgs; [
    # dev
    gcc # for `treesit-auto-install-all` which builds treesitter grammars
    gdb
    grafana-loki # provides: promtail loki-canary loki logcli
    nodejs # for copilot.el login

    omnisharp-roslyn # broken until https://github.com/NixOS/nixpkgs/pull/249091
    openssl
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
