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
    signal-desktop

    # video
    vlc

    # screenshot / screen capture
    flameshot
    gnome.gnome-screenshot
    obs-studio
    peek
    simplescreenrecorder

    # image manipulation
    inkscape
    gimp

    # volume control
    pavucontrol

    # Tools for monitoring the health of hard drives
    smartmontools
  ];
}
