{ pkgs, ... }: {
  home.packages = with pkgs; [
    bc
    cachix
    clang-tools
    du-dust
    exa
    flameshot
    gcc
    gimp
    git-crypt
    glances
    gnome3.gnome-screenshot
    gopass
    hunspellDicts.en_US-large # spellcheck dictionary used by libreoffice
    inkscape
    keepassxc
    libreoffice-fresh
    mkpasswd
    nixfmt
    okular
    pavucontrol
    pdfgrep
    peek
    procs
    python3
    python38Packages.mypy
    racket
    shfmt
    simple-scan
    syncthing-cli # provides stcli
    tldr
    trash-cli
    unar
    unzip
    vlc
    xorg.xkill
    zip
  ];
}
