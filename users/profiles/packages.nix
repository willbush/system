{ pkgs, ... }: {
  home.packages = with pkgs; [
    bc
    cachix
    clang
    clang-tools
    du-dust
    exa
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
    shfmt
    simple-scan
    syncthing-cli # provides stcli
    tldr
    trash-cli
    vlc
    xorg.xkill

    # compressor / archiver packages
    p7zip
    unar
    unzip
    zip
  ];
}
