{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # core
    curl
    eza
    fd
    ripgrep
    tree
    wget

    # dev
    gh
    nixfmt-rfc-style
    nodePackages.prettier

    # security
    git-crypt
    gopass

    # compressor / archiver packages
    unar
    unzip
    zip

    # other utils
    du-dust
    tealdeer # tldr in Rust
    trash-cli
    usbutils
  ];
}
