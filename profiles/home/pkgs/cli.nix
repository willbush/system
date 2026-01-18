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
  hyprcwd = writeScriptBin "hyprcwd" (builtins.readFile ../scripts/hyprcwd.sh);
in
{
  home.packages = with pkgs; [
    # Utilities
    curl
    dust
    eza
    fd
    ffmpeg
    file
    grim # Grab images from a Wayland compositor.
    lsof
    pinentry-gnome3
    ripgrep
    sd # sed alternative
    slurp # Select a region in a Wayland compositor (used with grim)
    tealdeer # tldr in Rust
    trash-cli
    tree
    unar
    unzip
    viu # cli image viewer (used by fzf-lua)
    usbutils
    wget
    wl-clipboard-rs
    wl-screenrec
    zip
    zola # blogging

    # Development
    bacon # watches your rust project and runs jobs in background.
    cargo-expand
    cargo-show-asm
    difftastic
    git-absorb
    glab
    hyperfine # benchmarking tool
    jjui
    lazyjj
    lldb # Debugger
    mergiraf # Syntax-aware git merge driver
    nix-prefetch-git
    repomix
    rust-analyzer
    tokei
    xxd # hexdump

    # Language formatters
    nixfmt-rfc-style
    nodePackages.prettier
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
    yq-go # YAML processor

    # Monitoring
    glances
    ncpamixer # mixer for PulseAudio inspired by pavucontrol
    nethogs
    nix-inspect
    nvtopPackages.amd
    viddy # Modern watch command
    zenith

    # Network
    awscli2
    dnsutils
    doggo # DNS Client for Humans.
    openconnect
    rustscan
    trippy
    xh

    # Custom scripts
    rsync-diff-home
    rsync-diff-root
    rsync-find-orphaned-files
    hyprcwd
  ];
}
