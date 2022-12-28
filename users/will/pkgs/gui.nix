{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # browsers
    (firefox.override { extraNativeMessagingHosts = [ browserpass ]; })
    ungoogled-chromium

    # chat
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

    # other
    hicolor-icon-theme # fall back icon theme
    mpv-unwrapped
    pkgs.stable.remmina
    rustdesk
    spotify
    transmission-gtk
    virt-manager
  ];
}
