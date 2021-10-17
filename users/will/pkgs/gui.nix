{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # browsers
    (firefox.override { extraNativeMessagingHosts = [ browserpass ]; })
    ungoogled-chromium

    # chat
    discord
    slack
    tdesktop # telegram desktop
    teams

    # other
    hicolor-icon-theme # fall back icon theme
    mpv-unwrapped
    remmina
    sxiv
    transmission-gtk
    virt-manager
  ];
}
