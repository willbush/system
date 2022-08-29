{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # browsers
    (firefox.override { extraNativeMessagingHosts = [ browserpass ]; })
    ungoogled-chromium

    # chat
    slack

    # image viewer
    gnome.eog
    sxiv

    # video editor
    libsForQt5.kdenlive

    # dev
    pkgs.stable.jetbrains.rider

    # Open source Dropbox client
    maestral-gui
    maestral # cli tool

    # other
    hicolor-icon-theme # fall back icon theme
    mpv-unwrapped
    pkgs.stable.remmina
    rustdesk
    transmission-gtk
    virt-manager
  ];
}
