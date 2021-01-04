{ pkgs, ... }: {
  fonts = {
    fonts = with pkgs; [
      dejavu_fonts
      emacs-all-the-icons-fonts
      hack-font
      inconsolata
      iosevka
      liberation_ttf
      powerline-fonts
      source-code-pro
      ubuntu_font_family
    ];
  };
}
