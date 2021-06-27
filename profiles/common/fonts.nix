{ pkgs, ... }: {
  fonts = {
    fonts = with pkgs; [
      dejavu_fonts
      emacs-all-the-icons-fonts
      fira-mono
      hack-font
      inconsolata
      iosevka
      liberation_ttf
      nerdfonts
      powerline-fonts
      source-code-pro
      ubuntu_font_family
    ];
  };
}
