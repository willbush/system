{ pkgs, ... }: {
  home.packages = with pkgs; [
    bc
    cachix
    clang
    clang-tools
    curl
    du-dust
    exa
    fd
    git-crypt
    glances
    gopass
    hunspellDicts.en_US-large # spellcheck dictionary used by libreoffice
    mkpasswd
    mutagen
    nixpkgs-fmt
    pdfgrep
    procs
    python3
    python38Packages.mypy
    racket
    ripgrep
    shfmt
    syncthing-cli # provides stcli
    tldr
    trash-cli
    tree
    wget
    xorg.xkill

    # compressor / archiver packages
    p7zip
    unar
    unzip
    zip
  ];
}
