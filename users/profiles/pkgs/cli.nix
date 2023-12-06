{ pkgs, ... }:
let
  planck = pkgs.callPackage ../../../keyboard-firmware/planck { };
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
    nixfmt
    nixpkgs-fmt
    nodePackages.prettier
    poetry
    python311
    python311Packages.grip # markdown preview
    python311Packages.pip
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
    # zenith #broken

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
