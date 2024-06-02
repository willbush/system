{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # office
    libreoffice-fresh
    hunspellDicts.en_US-large # spellcheck dictionary used by libreoffice
    simple-scan

    # security
    globalprotect-openconnect
    keepassxc

    # chat
    signal-desktop

    # video
    vlc

    # screenshot / screen capture
    grim
    slurp
    obs-studio
    peek

    # image manipulation
    inkscape
    gimp

    # browsers
    (firefox.override { nativeMessagingHosts = [ browserpass ]; })

    ungoogled-chromium

    # chat
    discord
    slack

    # video editor
    libsForQt5.kdenlive

    # dev
    jetbrains.rider

    # Open source Dropbox client
    maestral-gui
    maestral # cli tool

    # file manager
    gnome.nautilus
    # manage keyring
    gnome.seahorse

    # audio
    helvum # A GTK patchbay for pipewire.

    # other
    pkgs.transmission-gtk
    remmina
    virt-manager
    wireshark
  ];
}
