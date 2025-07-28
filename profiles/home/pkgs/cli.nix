{ pkgs, ... }:
let
  inherit (pkgs) writeScriptBin;
  rsync-diff-home = writeScriptBin "rsync-diff-home" (
    builtins.readFile ../scripts/rsync-diff-home.sh
  );
  rsync-diff-root = writeScriptBin "rsync-diff-root" (
    builtins.readFile ../scripts/rsync-diff-root.sh
  );
  rsync-find-orphaned-files = writeScriptBin "rsync-find-orphaned-files" (
    builtins.readFile ../scripts/rsync-find-orphaned-files.sh
  );
  # planck = pkgs.callPackage ../../../configs/keyboard-firmware/planck { };
  # crkbd = pkgs.callPackage ../../../configs/keyboard-firmware/crkbd { };
in
{
  home.packages = with pkgs; [
    # Utilities
    curl
    du-dust
    exiftool
    eza
    fd
    ffmpeg
    file
    grim # Grab images from a Wayland compositor.
    lsof
    mdbook
    nodePackages.prettier
    pinentry-gnome3
    ripgrep
    sd # sed alternative
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
    cargo-show-asm
    cargo-expand
    git-absorb
    glab
    hyperfine # benchmarking tool
    lldb # Debugger
    mergiraf # Syntax-aware git merge driver
    nix-prefetch-git
    rust-analyzer
    tig # text-mode interface for Git
    tokei
    trufflehog # Find, verify, and analyze leaked credentials (must be run with --no-update in NixOS)
    xxd # hexdump

    # AI cli
    (python3.withPackages (
      ps: with ps; [
        llm
        llm-gemini
      ]
    ))
    repomix

    # Language formatters
    nixfmt-rfc-style
    shfmt
    stylua

    # Language servers
    marksman # for markdown
    lua-language-server
    markdown-oxide
    nil
    taplo # TOML toolkit
    yaml-language-server

    # Cryptography
    age
    sops

    # Data processing
    jq
    xan # CSV
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
    trippy
    xh

    # Keyboard firmware flash
    # crkbd
    # planck

    # Custom scripts
    rsync-diff-home
    rsync-diff-root
    rsync-find-orphaned-files
  ];
}
