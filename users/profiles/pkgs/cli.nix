{ pkgs, ... }:
let
  planck = pkgs.callPackage ../../../configs/keyboard-firmware/planck { };
in
{
  home.packages = with pkgs; [
    planck
    # core
    curl
    eza
    fd
    ripgrep
    tree
    wget

    # dev
    delta
    gh
    jq
    nil # Nix Language server
    nixpkgs-fmt
    nodePackages.prettier
    omnisharp-roslyn # broken until https://github.com/NixOS/nixpkgs/pull/249091
    python311Packages.grip # markdown preview
    shfmt
    xq # jq in rust

    # security
    git-crypt
    gopass

    # compressor / archiver packages
    unar
    unzip
    zip

    # tui utils
    btop
    glances
    zenith

    # other utils
    du-dust
    file
    hyperfine # benchmarking tool
    tealdeer # tldr in Rust
    trash-cli
    usbutils
  ];
}
