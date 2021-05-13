{ pkgs, ... }: {
  home.packages = with pkgs; [
    albert
    bc
    cachix
    chromium
    clang-tools
    du-dust
    exa
    gcc
    gimp
    git-crypt
    glances
    gnome3.gnome-screenshot
    gnupg
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
    shutter
    simple-scan
    syncthing-cli # provides stcli
    teams
    tldr
    trash-cli
    unar
    unzip
    vlc
    xorg.xkill
    zip
  ];
}
