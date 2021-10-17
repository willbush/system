{ pkgs, ... }: {
  home.packages = with pkgs; [
    # office
    libreoffice-fresh
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
    gnome3.gnome-screenshot
    peek

    # image manipulation
    inkscape
    gimp

    # volume control
    pavucontrol
  ];
}
