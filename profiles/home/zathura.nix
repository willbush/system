{ ... }:
{
  programs.zathura = {
    enable = true;

    options = {
      recolor = true;
      recolor-keephue = true;
    };

    mappings = {
      "m" = "scroll left";
      "n" = "scroll down";
      "e" = "scroll up";
      "i" = "scroll right";

      "N" = "scroll full-down";
      "E" = "scroll full-up";

      "h" = "navigate next";
      "H" = "navigate previous";

      "[index] e" = "navigate_index up";
      "[index] i" = "navigate_index expand";
      "[index] m" = "navigate_index collapse";
      "[index] n" = "navigate_index down";

      "[index] <C-g>" = "toggle_index";
      "[insert] <C-g>" = "abort";
      "<C-g>" = "abort";

      "gr" = "reload";
    };
  };
}
