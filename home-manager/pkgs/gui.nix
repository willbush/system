{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # office
    hunspellDicts.en_US-large # spellcheck dictionary used by libreoffice
    libreoffice-fresh
    simple-scan

    # Security & Password Management
    globalprotect-openconnect # VPN
    keepassxc

    # Communication
    discord
    signal-desktop
    slack

    # Multimedia
    gimp
    grim
    inkscape
    libsForQt5.kdenlive
    obs-studio
    peek
    slurp
    vlc

    # Browsers
    (firefox.override { nativeMessagingHosts = [ browserpass ]; })
    ungoogled-chromium

    # Development
    jetbrains.rider

    # Cloud & Sync
    maestral-gui
    maestral # cli tool

    # GNOME Apps
    gnome.nautilus
    gnome.seahorse

    # Audio
    helvum # A GTK patchbay for pipewire.

    # Utilities
    pkgs.transmission-gtk
    remmina
    virt-manager
    wireshark
  ];
}
