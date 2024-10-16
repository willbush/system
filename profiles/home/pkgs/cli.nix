{ pkgs, ... }:
let
  # crkbd = pkgs.callPackage ../../../configs/keyboard-firmware/crkbd { };
  planck = pkgs.callPackage ../../../configs/keyboard-firmware/planck { };
in
{
  home.packages = with pkgs; [
    # Utilities
    curl
    du-dust
    eza
    fd
    file
    git-crypt
    gopass
    grim # Grab images from a Wayland compositor.
    lsof
    nixfmt-rfc-style
    nodePackages.prettier
    ripgrep
    slurp # Select a region in a Wayland compositor (used with grim)
    tealdeer # tldr in Rust
    trash-cli
    tree
    unar
    unzip
    usbutils
    wget
    wl-clipboard-rs
    wl-screenrec
    zip
    zola # blogging

    # Development
    gcc # for `treesit-auto-install-all` which builds treesitter grammars
    gdb
    hyperfine # benchmarking tool
    nix-prefetch-git
    omnisharp-roslyn # broken until https://github.com/NixOS/nixpkgs/pull/249091
    openssl
    python312Packages.grip # markdown preview
    quickemu # VM manager
    shfmt
    tokei
    trufflehog # Find, verify, and analyze leaked credentials (must be run with --no-update in NixOS)
    xxd # hexdump
    # language servers
    markdown-oxide
    nil
    yaml-language-server

    # Data processing
    jq
    xq # jq in rust
    yq-go # YAML processor

    # Monitoring
    glances
    ncpamixer # mixer for PulseAudio inspired by pavucontrol
    nethogs
    nix-inspect
    nvtopPackages.amd
    promql-cli
    viddy # Modern watch command
    zenith

    # Kubernetes
    argocd
    cilium-cli
    fluxcd
    grafana-loki # provides: promtail loki-canary loki logcli
    kind
    kubectl
    kubelogin-oidc # k8s credential plugin implementing OIDC auth (used by omni)
    kubernetes-helm
    kubeseal
    kustomize
    talosctl

    # Network
    dnsutils
    doggo # DNS Client for Humans.
    openconnect
    rustscan
    xh

    # Keyboard firmware flash
    # crkbd
    planck
  ];
}
