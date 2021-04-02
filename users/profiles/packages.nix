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
    keepassxc
    libreoffice-fresh
    logisim
    mkpasswd
    nixfmt
    okular
    pavucontrol
    pdfgrep
    peek
    python3
    racket
    shfmt
    shutter
    simple-scan
    syncthing-cli # provides stcli
    teams
    trash-cli
    unar
    unzip
    vlc
    xorg.xkill
    zip
  ];
}

