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
    nil # Nix Language server
    nix-prefetch-git
    nodejs # for copilot.el login
    omnisharp-roslyn # broken until https://github.com/NixOS/nixpkgs/pull/249091
    openssl
    python312Packages.grip # markdown preview
    quickemu # VM manager
    shfmt
    tokei

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
    viddy # Modern watch command
    zenith

    # Kubernetes
    argocd
    cilium-cli
    fluxcd
    grafana-loki # provides: promtail loki-canary loki logcli
    kubectl
    kubelogin-oidc # k8s credential plugin implementing OIDC auth (used by omni)
    kubernetes-helm
    kubeseal
    kustomize
    kind
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
