{ pkgs, ... }:
let
  # planck = pkgs.callPackage ../../../configs/keyboard-firmware/planck { };
  crkbd = pkgs.callPackage ../../../configs/keyboard-firmware/crkbd { };
in
{
  home.packages = with pkgs; [
    # Utilities
    curl
    du-dust
    eza
    fd
    file
    grim # Grab images from a Wayland compositor.
    lsof
    nixfmt-rfc-style
    nodePackages.prettier
    pinentry-gnome3
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
    bacon # watches your rust project and runs jobs in background.
    hyperfine # benchmarking tool
    nix-prefetch-git
    shfmt
    tokei
    trufflehog # Find, verify, and analyze leaked credentials (must be run with --no-update in NixOS)
    xxd # hexdump
    # language servers
    markdown-oxide
    nil
    yaml-language-server

    # Cryptography
    age
    sops

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

    # Network
    awscli
    dnsutils
    doggo # DNS Client for Humans.
    openconnect
    rustscan
    xh

    # Keyboard firmware flash
    crkbd
    # planck
  ];
}
