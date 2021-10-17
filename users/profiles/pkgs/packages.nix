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
    python38Packages.mypy
    racket
    shfmt

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
    glances
    procs

    # other utils
    du-dust
    mutagen
    pdfgrep
    tldr
    trash-cli
    xorg.xkill
  ];
}
