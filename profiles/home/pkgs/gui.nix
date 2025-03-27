{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # office
    foliate # A simple and modern GTK eBook reader
    libreoffice
    simple-scan

    # Communication
    signal-desktop
    slack
    vesktop # discord client

    # Multimedia
    gimp
    inkscape
    obs-studio
    spotify
    swappy # Wayland native snapshot editing tool
    vlc

    # Browsers
    firefox
    librewolf
    ungoogled-chromium # sometimes I use for testing

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
