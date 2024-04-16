{ pkgs, ... }:
let
  # crkbd = pkgs.callPackage ../../../configs/keyboard-firmware/crkbd { };
  planck = pkgs.callPackage ../../../configs/keyboard-firmware/planck { };
in
{
  home.packages = with pkgs; [
    # dev
    docker
    docker-compose
    gcc # for `treesit-auto-install-all` which builds treesitter grammars
    gdb
    grafana-loki # provides: promtail loki-canary loki logcli
    hyperfine # benchmarking tool
    nil # Nix Language server
    nix-prefetch-git
    nodejs # for copilot.el login
    omnisharp-roslyn # broken until https://github.com/NixOS/nixpkgs/pull/249091
    openssl
    python311Packages.grip # markdown preview
    shfmt
    tokei

    # jq like tools
    jq
    # xq # jq in rust
    yq # YAML/XML/TOML processor

    # k8s
    argocd
    k9s
    kube-score
    kubeconform
    kubectl
    kubelogin-oidc # k8s credential plugin implementing OIDC auth
    kubernetes-helm
    kustomize
    minikube
    talosctl

    # tui
    glances
    ncpamixer # mixer for PulseAudio inspired by pavucontrol
    nethogs
    nvtopPackages.amd
    zenith

    # pandoc related
    pandoc
    texlive.combined.scheme-small # things needed for pandoc

    # network
    dnsutils
    openconnect
    rustscan
    xh

    # Keyboard firmware flash
    # crkbd
    planck

    # other
    file
    lsof
    wl-clipboard-rs
    zola
  ];
}
