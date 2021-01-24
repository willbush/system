{ pkgs, ... }: {
  home.packages = with pkgs; [
    albert
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    bc
    cachix
    calibre
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
    hunspellDicts.en_US-large # used by libreoffice
    keepassxc
    libreoffice-fresh
    mkpasswd
    nixfmt
    okular
    pavucontrol
    pdfgrep
    peek
    python3
    shfmt
    shutter
    simple-scan
    syncthing-cli # provides stcli
    teams
    unar
    unzip
    vlc
    xorg.xkill
    zip
  ];
}
