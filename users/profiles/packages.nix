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
    flameshot
    gimp
    git-crypt
    glances
    globalprotect-openconnect
    gnome3.gnome-screenshot
    gopass
    hunspellDicts.en_US-large # spellcheck dictionary used by libreoffice
    inkscape
    keepassxc
    libreoffice-fresh
    mkpasswd
    mutagen
    nixpkgs-fmt
    okular
    pavucontrol
    pdfgrep
    peek
    procs
    python3
    python38Packages.mypy
    racket
    ripgrep
    shfmt
    simple-scan
    syncthing-cli # provides stcli
    tldr
    trash-cli
    tree
    vlc
    wget
    xorg.xkill

    # compressor / archiver packages
    p7zip
    unar
    unzip
    zip
  ];
}
