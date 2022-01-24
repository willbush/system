{ pkgs, ... }: {
  home.packages = with pkgs; [
    # office
    libreoffice-fresh
    hunspellDicts.en_US-large # spellcheck dictionary used by libreoffice
    okular
    simple-scan

    # security
    globalprotect-openconnect
    keepassxc

    # chat
    discord
    signal-desktop
    teams

    # video
    vlc

    # screenshot / screen capture
    flameshot
    gnome3.gnome-screenshot
    peek

    # image manipulation
    inkscape
    gimp

    # volume control
    pavucontrol

    # Tools for monitoring the health of hard drives
    smartmontools
  ];
}
