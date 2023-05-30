{ pkgs, ... }: {
  fonts = {
    fonts = with pkgs; [
      dejavu_fonts
      fira-mono
      hack-font
      inconsolata
      iosevka
      liberation_ttf
      libre-baskerville
      nerdfonts
      powerline-fonts
      source-code-pro
      ubuntu_font_family
    ];
  };
}
