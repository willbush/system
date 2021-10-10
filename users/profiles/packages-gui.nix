{ pkgs, ... }: {
  home.packages = with pkgs; [
    flameshot
    gimp
    globalprotect-openconnect
    gnome3.gnome-screenshot
    inkscape
    keepassxc
    libreoffice-fresh
    okular
    pavucontrol
    peek
    simple-scan
    vlc
  ];
}
