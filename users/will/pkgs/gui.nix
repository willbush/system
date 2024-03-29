{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # browsers
    (firefox.override { nativeMessagingHosts = [ browserpass ]; })

    ungoogled-chromium

    # chat
    discord
    slack

    # image viewer
    sxiv

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
    hicolor-icon-theme # fall back icon theme
    pkgs.transmission-gtk
    remmina
    virt-manager
    wireshark
  ];
}
