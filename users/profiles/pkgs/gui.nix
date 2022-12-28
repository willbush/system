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
    zoom-us

    # video
    vlc

    # screenshot / screen capture
    flameshot
    gnome.gnome-screenshot
    peek
    simplescreenrecorder

    # image manipulation
    inkscape
    gimp

    # volume control
    pavucontrol

    # Tools for monitoring the health of hard drives
    smartmontools

    # remove unnecessary files from your computer
    czkawka

    # Dev
    arduino
    vscodium
  ];
}
