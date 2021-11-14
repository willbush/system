{ pkgs, ... }: {
  home.packages = with pkgs; [
    # office
    libreoffice-fresh
    hunspellDicts.en_US-large # spellcheck dictionary used by libreoffice
    okular
    simple-scan

    # java
    jdk8
    jetbrains.idea-community

    # security
    globalprotect-openconnect
    keepassxc

    # chat
    signal-desktop

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
  ];
}
