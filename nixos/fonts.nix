{ pkgs, ... }:

{
  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      hack-font
      source-code-pro
      powerline-fonts
      corefonts
      dejavu_fonts
      font-droid
      freefont_ttf
      google-fonts
      inconsolata
      liberation_ttf
      terminus_font
      ttf_bitstream_vera
      ubuntu_font_family
    ];
  };
}
