{ pkgs, ... }:
let planck = pkgs.callPackage ../../../keyboard-firmware/planck { };
in
{
  home.packages = with pkgs; [
    android-tools
    dnsutils
    dotnet-sdk_5
    exiftool
    feh
    lsof
    neofetch
    nethogs
    niv
    nix-prefetch-git
    omnisharp-roslyn
    openconnect
    pandoc
    planck
    rust-analyzer
    rustup
    texlive.combined.scheme-small # things needed for pandoc
    tokei
    woeusb # Windows ISO to USB drive utility
    xclip
    xdotool
    zola
  ];
}
