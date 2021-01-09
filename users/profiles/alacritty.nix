{
  programs.alacritty = {
    enable = true;
    # Colors (One Dark)
    # setting examples: https://github.com/jwilm/alacritty/blob/master/alacritty.yml
    # themes: https://github.com/eendroroy/alacritty-theme
    settings = {
      colors = {
        # I changed the primary background color to black because I like that
        # with transparency.
        primary = {
          background = "0x000000";
          foreground = "0xabb2bf";
        };
        normal = {
          black = "0x1e2127";
          red = "0xe06c75";
          green = "0x98c379";
          yellow = "0xd19a66";
          blue = "0x61afef";
          magenta = "0xc678dd";
          cyan = "0x56b6c2";
          white = "0xabb2bf";
        };
        bright = {
          black = "0x5c6370";
          red = "0xe06c75";
          green = "0x98c379";
          yellow = "0xd19a66";
          blue = "0x61afef";
          magenta = "0xc678dd";
          cyan = "0x56b6c2";
          white = "0xffffff";
        };
      };
    };
  };
}
