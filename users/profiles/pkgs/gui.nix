{ pkgs, ... }: {
  home.packages = with pkgs; [
    # office
    libreoffice-fresh
    hunspellDicts.en_US-large # spellcheck dictionary used by libreoffice
    okular
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
  ];
}
