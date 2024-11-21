{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # office
    foliate # A simple and modern GTK eBook reader
    libreoffice
    simple-scan

    # Communication
    signal-desktop
    vesktop # discord client

    # Multimedia
    gimp
    inkscape
    obs-studio
    swappy # Wayland native snapshot editing tool
    vlc

    # Browsers
    firefox
    chromium # sometimes I use for testing

    # GNOME Apps
    nautilus # file manager I sometimes use
    seahorse # password and keys manager

    # Audio
    helvum # A GTK patchbay for pipewire.
    pavucontrol

    # Utilities
    transmission_4-gtk
    wireshark
  ];
}
