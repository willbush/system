{
  programs.less = {
    enable = true;
    # Rebind to my custom vim bindings for Colemak-DHm
    commands = {
      n = "forw-line";
      e = "back-line";
      N = "forw-scroll";
      E = "back-scroll";
      h = "repeat-search";
      H = "reverse-search";
      "^H" = "help"; # This syntax means C-h
      k = "set-mark";
      K = "set-mark-bottom";
      j = "goto-mark";
    };
  };
}
