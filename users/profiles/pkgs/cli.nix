{ pkgs, ... }: {
  home.packages = with pkgs; [
    # core
    curl
    exa
    fd
    ripgrep
    tree
    wget

    # dev
    clang
    clang-tools
    nixpkgs-fmt
    python3
    racket
    shfmt

    # node
    nodejs
    nodePackages.npm
    nodePackages.expo-cli
    nodePackages.typescript
    nodePackages.typescript-language-server

    # security
    git-crypt
    gopass
    mkpasswd

    # compressor / archiver packages
    p7zip
    unar
    unzip
    zip

    # tui utils
    btop
    glances
    procs
    zenith

    # other utils
    android-tools
    du-dust
    mutagen
    pdfgrep
    tldr
    trash-cli
    xorg.xkill
  ];
}
