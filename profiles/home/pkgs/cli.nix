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
  sc2reader = pkgs.python3Packages.buildPythonPackage rec {
    pname = "sc2reader";
    version = "1.9.0";
    pyproject = true;
    build-system = [ pkgs.python3Packages.setuptools ];
    src = pkgs.fetchPypi {
      inherit pname version;
      hash = "sha256-kb5eRl7fKUdc8uT41KUv9MKKA4caIMHBq4iuW7p6qaI=";
    };
    dependencies = with pkgs.python3Packages; [ mpyq pillow ];
    doCheck = false;
  };
  sc2-practice-replays = pkgs.writers.writePython3Bin "sc2-practice-replays" {
    libraries = [ sc2reader ];
  } (builtins.readFile ../scripts/sc2-practice-replays.py);
in
{
  home.packages = with pkgs; [
    # Utilities
    curl
    dust
    ente-cli
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
    usbutils
    viu # cli image viewer (used by fzf-lua)
    wget
    wl-clipboard-rs
    wl-screenrec
    zip

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
    tokei
    xxd # hexdump

    # Language formatters
    nixfmt
    prettier
    shfmt
    stylua

    # Language servers
    marksman # for markdown
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
    sc2-practice-replays
  ];
}
