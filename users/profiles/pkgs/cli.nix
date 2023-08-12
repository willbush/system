{ pkgs, ... }:
let
  planck = pkgs.callPackage ../../../keyboard-firmware/planck { };
in
{
  home.packages = with pkgs; [
    planck
    # core
    curl
    exa
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
    python311
    python311Packages.grip # markdown preview
    shfmt
    terraform
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
