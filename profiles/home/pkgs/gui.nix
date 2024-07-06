{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # office
    hunspellDicts.en_US-large # spellcheck dictionary used by libreoffice
    libreoffice-fresh
    simple-scan

    # Communication
    discord
    signal-desktop
    slack

    # Multimedia
    gimp
    inkscape
    obs-studio
    swappy # Wayland native snapshot editing tool
    vlc

    # Browsers
    (firefox.override { nativeMessagingHosts = [ browserpass ]; })
    chromium # sometimes I use for testing

    # Development
    jetbrains.rider

    # Cloud & Sync
    maestral-gui
    maestral # cli tool

    # GNOME Apps
    gnome.nautilus # file manager I sometimes use
    gnome.seahorse # password and keys manager

    # Audio
    helvum # A GTK patchbay for pipewire.

    # Utilities
    transmission_4-gtk
    remmina
    virt-manager
  ];
}
