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
    feh
    gcc
    gimp
    git-crypt
    glances
    gnome3.gnome-screenshot
    gnupg
    gopass
    keepassxc
    libreoffice
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
    unar
    unzip
    vlc
    xorg.xkill
    zip
  ];
}
