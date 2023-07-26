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
    python3
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
    bottom
    btop
    glances
    procs
    zenith

    # other utils
    onefetch # neofetch like tool
    du-dust
    file
    hyperfine # benchmarking tool
    mutagen
    pdfgrep
    tealdeer # tldr in Rust
    trash-cli
    usbutils
    xorg.xkill
  ];
}
