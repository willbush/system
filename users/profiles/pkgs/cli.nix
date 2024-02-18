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
    nixpkgs-fmt
    nodePackages.prettier
    python311Packages.grip # markdown preview
    shfmt
    xq # jq in rust

    # security
    git-crypt
    gopass

    # compressor / archiver packages
    p7zip
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
    xorg.xkill
  ];
}
