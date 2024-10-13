{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # office
    foliate # A simple and modern GTK eBook reader
    libreoffice-fresh
    simple-scan

    # Communication
    signal-desktop
    slack
    vesktop # discord client
    zoom-us

    # Multimedia
    gimp
    inkscape
    obs-studio
    swappy # Wayland native snapshot editing tool
    vlc

    # Browsers
    firefox
    chromium # sometimes I use for testing

    # Development
    jetbrains.rider

    # Cloud & Sync
    maestral-gui
    maestral # cli tool

    # GNOME Apps
    nautilus # file manager I sometimes use
    seahorse # password and keys manager

    # Audio
    helvum # A GTK patchbay for pipewire.
    pavucontrol

    # Utilities
    transmission_4-gtk
    remmina
    wireshark
  ];
}
