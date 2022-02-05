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

    # dev
    jetbrains.rider

    # other
    hicolor-icon-theme # fall back icon theme
    mpv-unwrapped
    remmina
    transmission-gtk
    virt-manager
  ];
}
