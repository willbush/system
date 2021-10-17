{ pkgs, ... }:
let planck = pkgs.callPackage ../../../keyboard-firmware/planck { };
in
{
  home.packages = with pkgs; [
    # dev
    dotnet-sdk_5
    nix-prefetch-git
    omnisharp-roslyn
    rust-analyzer
    rustup
    tokei

    # tui
    nethogs

    # pandoc releted
    pandoc
    texlive.combined.scheme-small # things needed for pandoc

    # other
    android-tools
    dnsutils
    exiftool
    feh
    lsof
    neofetch
    niv
    openconnect
    planck
    woeusb # Windows ISO to USB drive utility
    xclip
    xdotool
    zola
  ];
}
