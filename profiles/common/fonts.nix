{ pkgs, ... }: {
  fonts = {
    fonts = with pkgs; [
      corefonts
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
  modules.unfree.allowList = [
    "corefonts"
  ];
}
